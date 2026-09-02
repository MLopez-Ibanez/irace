#!/usr/bin/env julia

# ------------------------------------------------------------------
# Arguments supplied by irace:
#
# ARGS[1] = configurationID
# ARGS[2] = instanceID
# ARGS[3] = seed
# ARGS[4] = instance
# ARGS[5] = bound
#
# Then come the parameters from parameters.txt.
# ------------------------------------------------------------------
include("solver.jl")

configuration_id = parse(Int, ARGS[1])
instance_id      = parse(Int, ARGS[2])
seed             = parse(Int, ARGS[3])
instance         = ARGS[4]
bound            = parse(Float64, ARGS[5])

# Parse remaining --key value arguments.
params = Dict{String,String}()

i = 6
while i <= length(ARGS)
    key = ARGS[i]

    if !startswith(key, "--")
        error("Expected parameter switch, got: $key")
    end

    if i == length(ARGS)
        error("Missing value for $key")
    end

    params[key[3:end]] = ARGS[i + 1]
    i += 2
end

alpha      = parse(Float64, params["alpha"])
beta       = parse(Float64, params["beta"])
iterations = parse(Int, params["iterations"])

cost = solve(
    instance,
    alpha,
    beta,
    iterations,
    seed,
)

# ------------------------------------------------------------------
# irace output
# ------------------------------------------------------------------

println(cost)
