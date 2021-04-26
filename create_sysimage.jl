using PackageCompiler
create_sysimage([:Pluto, :PlutoUI, :Distributions, :Plots, :StatsPlots];
                precompile_execution_file = "warmup.jl",
                replace_default = false,
                sysimage_path = "PlotsSysimage.so",
                cpu_target = PackageCompiler.default_app_cpu_target())
