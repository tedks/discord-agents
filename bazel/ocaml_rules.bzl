"""Small explicit OCaml bytecode rules for Discord Agents project parsing."""

OcamlInfo = provider(fields = ["objects", "interfaces", "includes", "stdlib_libraries"])

def _compiler_files(ctx):
    files = ctx.files.compiler
    compiler = [f for f in files if f.basename == "ocamlc"]
    runtime = [f for f in files if f.basename == "ocamlrun"]
    libdirs = [f for f in files if f.basename == "ocaml" and f.is_directory]
    if len(compiler) != 1 or len(runtime) != 1 or len(libdirs) != 1:
        fail("OCaml compiler must publish ocamlc, ocamlrun, and lib/ocaml")
    return struct(compiler = compiler[0], runtime = runtime[0], libdir = libdirs[0], files = files)

def _dependency_inputs(deps):
    objects = []
    interfaces = []
    includes = []
    stdlib_libraries = []
    for dep in deps:
        info = dep[OcamlInfo]
        objects.extend(info.objects)
        interfaces.extend(info.interfaces)
        includes.extend(info.includes)
        stdlib_libraries.extend(info.stdlib_libraries)
    return struct(objects = objects, interfaces = interfaces, includes = includes, stdlib_libraries = stdlib_libraries)

def _ocaml_library_impl(ctx):
    tool = _compiler_files(ctx)
    deps = _dependency_inputs(ctx.attr.deps)
    stem = ctx.file.src.basename[:-3]
    obj = ctx.actions.declare_file(stem + ".cmo")
    interface = ctx.actions.declare_file(stem + ".cmi")
    args = ctx.actions.args()
    args.add("-I", tool.libdir.path)
    for library in ctx.attr.stdlib_libraries:
        args.add("-I", "+" + library)
    for directory in deps.includes:
        args.add("-I", directory)
    args.add("-c", ctx.file.src.path)
    args.add("-o", obj.path)
    ctx.actions.run(
        executable = tool.compiler,
        arguments = [args],
        inputs = [ctx.file.src] + deps.objects + deps.interfaces,
        tools = tool.files,
        outputs = [obj, interface],
        env = {"OCAMLLIB": tool.libdir.path},
        mnemonic = "OcamlCompile",
    )
    return [
        DefaultInfo(files = depset([obj, interface])),
        OcamlInfo(
            objects = deps.objects + [obj],
            interfaces = deps.interfaces + [interface],
            includes = deps.includes + [obj.dirname],
            stdlib_libraries = deps.stdlib_libraries + ctx.attr.stdlib_libraries,
        ),
    ]

ocaml_library = rule(
    implementation = _ocaml_library_impl,
    attrs = {
        "src": attr.label(allow_single_file = [".ml"], mandatory = True),
        "deps": attr.label_list(providers = [OcamlInfo]),
        "stdlib_libraries": attr.string_list(),
        "compiler": attr.label(default = "//:ocaml_compiler"),
    },
)

def _ocaml_test_impl(ctx):
    tool = _compiler_files(ctx)
    deps = _dependency_inputs(ctx.attr.deps)
    bytecode = ctx.actions.declare_file(ctx.label.name + ".byte")
    args = ctx.actions.args()
    args.add("-I", tool.libdir.path)
    for library in deps.stdlib_libraries + ctx.attr.stdlib_libraries:
        args.add("-I", "+" + library)
    for directory in deps.includes:
        args.add("-I", directory)
    args.add("-o", bytecode.path)
    for library in deps.stdlib_libraries + ctx.attr.stdlib_libraries:
        args.add(library + ".cma")
    args.add_all([obj.path for obj in deps.objects])
    args.add(ctx.file.src.path)
    ctx.actions.run(
        executable = tool.compiler,
        arguments = [args],
        inputs = [ctx.file.src] + deps.objects + deps.interfaces,
        tools = tool.files,
        outputs = [bytecode],
        env = {"OCAMLLIB": tool.libdir.path},
        mnemonic = "OcamlLinkTest",
    )
    launcher = ctx.actions.declare_file(ctx.label.name + ".sh")
    workspace = ctx.workspace_name
    ctx.actions.write(
        launcher,
        "#!/bin/sh\nset -eu\nroot=${RUNFILES_DIR:-\"$0.runfiles\"}\nexec \"$root/%s/%s\" \"$root/%s/%s\"\n" % (
            workspace,
            tool.runtime.short_path,
            workspace,
            bytecode.short_path,
        ),
        is_executable = True,
    )
    return DefaultInfo(
        executable = launcher,
        runfiles = ctx.runfiles(files = [tool.runtime, bytecode]),
    )

ocaml_test = rule(
    implementation = _ocaml_test_impl,
    test = True,
    attrs = {
        "src": attr.label(allow_single_file = [".ml"], mandatory = True),
        "deps": attr.label_list(providers = [OcamlInfo]),
        "stdlib_libraries": attr.string_list(),
        "compiler": attr.label(default = "//:ocaml_compiler"),
    },
)
