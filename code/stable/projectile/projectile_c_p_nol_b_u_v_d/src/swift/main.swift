/** main.swift
    Controls the flow of the program
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
var filename: String = CommandLine.arguments[0]
var g: Double = 9.8
var epsilon: Double = 2.0e-2
var inParams: InputParameters = try InputParameters(filename)
var t_flight: Double = func_t_flight(&inParams, g)
var p_land: Double = func_p_land(&inParams, g)
var d_offset: Double = func_d_offset(&inParams, p_land)
var s: String = func_s(&inParams, epsilon, d_offset)
try write_output(s, d_offset, t_flight)
