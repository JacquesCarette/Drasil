module Defs where

import Language.Drasil.Code

-- gool labels/variables/params

l_a, l_b, l_t, l_gt, l_w, l_tnt, l_sdx, l_sdy,
  l_sdz, l_pbtol, l_asprat, l_sd, l_h, l_gtf, l_ldf, l_wtnt, l_E,
  l_td, l_m, l_k, l_lsf, l_w_array, l_data_sd, l_data_q, l_j_array, 
  l_data_asprat, l_data_qstar, l_params, l_q, l_j_tol, l_q_hat, l_q_hat_tol,
  l_pb, l_j, l_nfl, l_lr, l_is_safe1, l_is_safe2, l_y1, l_y2, l_x1, l_x2, 
  l_x, l_y, l_arr, l_v, l_i, l_mat, l_c, l_col, l_x_array, l_y_array, 
  l_z_array, l_z, l_x_z1, l_y_z1, l_x_z2, l_y_z2, l_y_upper, l_y_lower :: Label
l_a = "a"
l_b = "b"
l_t = "t"
l_gt = "gt"
l_w = "w"
l_tnt = "tnt"
l_sdx = "sdx"
l_sdy = "sdy"
l_sdz = "sdz"
l_pbtol = "pbtol"
l_asprat = "asprat"
l_sd = "sd"
l_h = "h"
l_gtf = "gtf"
l_ldf = "ldf"
l_wtnt = "wtnt"
l_E = "E"
l_td = "td"
l_m = "m"
l_k = "k"
l_lsf = "lsf"
l_w_array = "w_array"
l_data_sd = "data_sd"
l_data_q = "data_q"
l_j_array = "j_array"
l_data_asprat = "data_asprat"
l_data_qstar = "data_qstar"
l_params = "params"   
l_q = "q"
l_j_tol = "j_tol"
l_q_hat = "q_hat"
l_q_hat_tol = "q_hat_tol"
l_pb = "pb"
l_j = "j"
l_nfl = "nfl"
l_lr = "lr"
l_is_safe1 = "is_safe1"
l_is_safe2 = "is_safe2"
l_x1 = "x1" 
l_y1 = "y1" 
l_x2 = "x2"
l_y2 = "y2" 
l_x = "x"
l_y = "y"
l_arr = "arr"
l_v = "v"
l_i = "i"
l_mat = "mat"
l_c = "c"
l_col = "col"
l_x_array = "x_array"
l_y_array = "y_array"
l_z_array = "z_array"
l_z = "z"
l_x_z1 = "x_z1"
l_y_z1 = "y_z1"
l_x_z2 = "x_z2"
l_y_z2 = "y_z2"
l_y_upper = "y_upper"
l_y_lower = "y_lower"


v_a, v_b, v_t, v_gt, v_w, v_tnt, v_sdx, v_sdy,
  v_sdz, v_pbtol, v_asprat, v_sd, v_h, v_gtf, v_ldf, v_wtnt,
  v_E, v_td, v_m, v_k, v_lsf, v_w_array, v_data_sd, v_data_q, 
  v_j_array, v_data_asprat, v_data_qstar, v_params, v_q, v_j_tol, 
  v_q_hat, v_q_hat_tol, v_pb, v_j, v_nfl, v_lr, v_is_safe1, 
  v_is_safe2, v_x1, v_y1, v_x2, v_y2, v_x, v_y, v_arr, v_v, v_i, 
  v_mat, v_c, v_col, v_x_array, v_y_array, v_z_array, v_z, v_x_z1, 
  v_y_z1, v_x_z2, v_y_z2, v_y_upper, v_y_lower  :: Value
v_a = var l_a
v_b = var l_b
v_t = var l_t
v_gt = var l_gt
v_w = var l_w
v_tnt = var l_tnt
v_sdx = var l_sdx
v_sdy = var l_sdy
v_sdz = var l_sdz
v_pbtol = var l_pbtol
v_asprat = var l_asprat
v_sd = var l_sd
v_h = var l_h
v_gtf = var l_gtf
v_ldf = var l_ldf
v_wtnt = var l_wtnt
v_E = var l_E
v_td = var l_td
v_m = var l_m
v_k = var l_k
v_lsf = var l_lsf
v_w_array = var l_w_array
v_data_sd = var l_data_sd
v_data_q = var l_data_q
v_j_array = var l_j_array
v_data_asprat = var l_data_asprat
v_data_qstar = var l_data_qstar
v_params = var l_params
v_q = var l_q
v_j_tol = var l_j_tol
v_q_hat = var l_q_hat
v_q_hat_tol = var l_q_hat_tol
v_pb = var l_pb
v_j = var l_j
v_nfl = var l_nfl
v_lr = var l_lr
v_is_safe1 = var l_is_safe1
v_is_safe2 = var l_is_safe2
v_x1 = var l_x1
v_y1 = var l_y1
v_x2 = var l_x2
v_y2 = var l_y2
v_x = var l_x
v_y = var l_y
v_arr = var l_arr
v_v = var l_v
v_i = var l_i
v_mat = var l_mat
v_c = var l_c
v_col = var l_col
v_x_array = var l_x_array
v_y_array = var l_y_array
v_z_array = var l_z_array
v_z = var l_z
v_x_z1 = var l_x_z1
v_y_z1 = var l_y_z1
v_x_z2 = var l_x_z2
v_y_z2 = var l_y_z2
v_y_upper = var l_y_upper
v_y_lower = var l_y_lower

p_w_array, p_data_sd, p_data_q, p_params, p_q, p_j, p_q_hat_tol, 
  p_nfl, p_pb, p_lr, p_x1, p_y1, p_x2, p_y2, p_x, p_arr, p_v, p_mat, 
  p_c, p_x_array, p_y_array, p_z_array, p_z, p_y :: Parameter
p_w_array = param l_w_array (listT float)
p_data_sd = param l_data_sd (listT $ listT float)
p_data_q = param l_data_q (listT $ listT float)
p_params = param l_params (obj "InputParameters")
p_q = param l_q float
p_j = param l_j float
p_q_hat_tol = param l_q_hat_tol float
p_nfl = param l_nfl float
p_pb = param l_pb float
p_lr = param l_lr float
p_x1 = param l_x1 float
p_y1 = param l_y1 float
p_x2 = param l_x2 float
p_y2 = param l_y2 float
p_x = param l_x float
p_arr = param l_arr (listT float)
p_v = param l_v float
p_mat = param l_mat (listT $ listT float)
p_c = param l_c int
p_x_array = param l_x_array (listT $ listT float)
p_y_array = param l_y_array (listT $ listT float)
p_z_array = param l_z_array (listT float)
p_z = param l_z float
p_y = param l_y float