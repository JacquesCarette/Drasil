""" Calculations.jl
    Provides functions for calculating the outputs
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    - Note: Generated by Drasil v0.1-alpha
"""

module Calculations

""" Calculates flight duration: the time when the projectile lands (s)
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter g: magnitude of gravitational acceleration: the magnitude of the approximate acceleration due to gravity on Earth at sea level (m/s^2)
    - Returns: flight duration: the time when the projectile lands (s)
"""
function func_t_flight(v_launch::Float32, theta::Float32, g::Float32)
    return Float32(2.0) * v_launch * sin(theta) / g
end

""" Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter g: magnitude of gravitational acceleration: the magnitude of the approximate acceleration due to gravity on Earth at sea level (m/s^2)
    - Returns: landing position: the distance from the launcher to the final position of the projectile (m)
"""
function func_p_land(v_launch::Float32, theta::Float32, g::Float32)
    return Float32(2.0) * v_launch ^ Float32(2.0) * sin(theta) * cos(theta) / g
end

""" Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter p_target: target position: the distance from the launcher to the target (m)
    - Parameter p_land: landing position: the distance from the launcher to the final position of the projectile (m)
    - Returns: distance between the target position and the landing position: the offset between the target position and the landing position (m)
"""
function func_d_offset(p_target::Float32, p_land::Float32)
    return p_land - p_target
end

""" Calculates output message as a string
    - Parameter p_target: target position: the distance from the launcher to the target (m)
    - Parameter epsilon: hit tolerance
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Returns: output message as a string
"""
function func_s(p_target::Float32, epsilon::Float32, d_offset::Float32)
    if abs(d_offset / p_target) < epsilon
        return "The target was hit."
    elseif d_offset < Float32(0.0)
        return "The projectile fell short."
    else
        return "The projectile went long."
    end
end

end
