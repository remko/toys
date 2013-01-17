vars = Variables()
vars.Add(BoolVariable("test", "Run tests", "no"))

env = Environment(variables = vars)
Help(vars.GenerateHelpText(env))

Export("env")

SConscript(dirs = [
	"c++",
    "java",
])
