/** Calculations.swift
    Provides functions for calculating the outputs
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

/** Calculates flight duration: the time when the projectile lands (s)
    - Parameter inParams: structure holding the input values
    - Parameter g: magnitude of gravitational acceleration (m/s^2)
    - Returns: flight duration: the time when the projectile lands (s)
*/
func func_t_flight(_ inParams: inout InputParameters, _ g: Double) -> Double {
    return 2.0 * inParams.v_launch * sin(inParams.theta) / g
}

/** Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    - Parameter inParams: structure holding the input values
    - Parameter g: magnitude of gravitational acceleration (m/s^2)
    - Returns: landing position: the distance from the launcher to the final position of the projectile (m)
*/
func func_p_land(_ inParams: inout InputParameters, _ g: Double) -> Double {
    return 2.0 * pow(inParams.v_launch, 2.0) * sin(inParams.theta) * cos(inParams.theta) / g
}

/** Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter inParams: structure holding the input values
    - Parameter p_land: landing position: the distance from the launcher to the final position of the projectile (m)
    - Returns: distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
func func_d_offset(_ inParams: inout InputParameters, _ p_land: Double) -> Double {
    return p_land - inParams.p_target
}

/** Calculates output message as a string
    - Parameter inParams: structure holding the input values
    - Parameter epsilon: hit tolerance
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Returns: output message as a string
*/
func func_s(_ inParams: inout InputParameters, _ epsilon: Double, _ d_offset: Double) -> String {
    if abs(d_offset / inParams.p_target) < epsilon {
        return "The target was hit."
    }
    else if d_offset < 0.0 {
        return "The projectile fell short."
    }
    else {
        return "The projectile went long."
    }
}
