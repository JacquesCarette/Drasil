/** Calculations.swift
    Provides functions for calculating the outputs
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

/** Calculates flight duration: the time when the projectile lands (s)
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter g: magnitude of gravitational acceleration (m/s^2)
    - Returns: flight duration: the time when the projectile lands (s)
*/
func func_t_flight(_ v_launch: Float, _ theta: Float, _ g: Float) -> Float {
    return 2.0 * v_launch * sin(theta) / g
}

/** Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter g: magnitude of gravitational acceleration (m/s^2)
    - Returns: landing position: the distance from the launcher to the final position of the projectile (m)
*/
func func_p_land(_ v_launch: Float, _ theta: Float, _ g: Float) -> Float {
    return 2.0 * pow(v_launch, 2.0) * sin(theta) * cos(theta) / g
}

/** Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter p_target: target position: the distance from the launcher to the target (m)
    - Parameter p_land: landing position: the distance from the launcher to the final position of the projectile (m)
    - Returns: distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
func func_d_offset(_ p_target: Float, _ p_land: Float) -> Float {
    return p_land - p_target
}

/** Calculates output message as a string
    - Parameter p_target: target position: the distance from the launcher to the target (m)
    - Parameter epsilon: hit tolerance
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Returns: output message as a string
*/
func func_s(_ p_target: Float, _ epsilon: Float, _ d_offset: Float) -> String {
    if abs(d_offset / p_target) < epsilon {
        return "The target was hit."
    }
    else if d_offset < 0.0 {
        return "The projectile fell short."
    }
    else {
        return "The projectile went long."
    }
}
