"""
Interpolation Module
Secret: The interpolation algorithm.
Service: Provides the equations that take the input parameters and interpolation
data and return an interpolated value.
"""

import numpy as np


def lin_interp(y1, y2, x1, x2, input_param):
    """
    Defines the equation for the linear interpolation.
    """
    y0 = y1 + (y2 - y1)/(x2 - x1)*(input_param - x1)
    return y0


def proper_index(index1,index2,data,value):
    if index1 == 0:
        if data[index1,index2] == data[index1+1,index2]:
            index1 += 1
    elif index1 == (data[:,index2]).argmax():
        index1 -= 1
    else:
        if data[index1,index2] > value:
            index1 -= 1
        elif data[index1,index2] < value:
            if (index1+1 < (data[:,index2]).argmax()) and (data[index1,index2] == data[index1+1,index2]):
                index1 += 1
            elif (index1+1 == (data[:,index2]).argmax()) and (data[index1,index2] == data[index1+1,index2]):
                index1 -= 1 
    return index1 


# find_bounds: numpy.ndarray numpy.ndarray Num Num -> Int Int Int Int Int
def find_bounds(data1, data2, value1, value2):
    """
    Finds the closest values above and below the input parameter to be used in
    the interpolation.  Also returns how many interpolations are to be performed.
    """
    # idx represents the index of the closest value of value1 in the array data1(e.g. for w_array,idx=0 if input w = 4.5)
    idx = (np.abs(data1 - value1)).argmin()
    # if the closest value is greater than the input parameter value1, we let idx=idx-1 to ensure the input parameter is 
    #   within the range [closest value below value1(data1[idx]), closest value above value1(data1[idx+1])]
    if data1[idx] > value1:
        idx -= 1
    # jdx represents the index of the closest value of value2 in the ith column of the array data2(data2[:,idx])
    jdx = (np.abs(data2[:, idx] - value2)).argmin()
    # kdx represents the index of the closest value of value2 in the (i+1)th column of the array data2(data2[:,idx+1])
    kdx = (np.abs(data2[:, idx+1] - value2)).argmin() 
    # Case1: value1 in data1; num_interp1 = 0; look for value2 in data2[:,idx]
    if value1 in data1:
        num_interp1 = 0
        # Case1_1: value2 in data2[:,idx]; num_interp2 = 0
        if value2 in data2[:,idx]:
            num_interp2 = 0
        # Case1_2: value2 NOT in data2[:,idx]; num_interp2 = 1
        else:
            num_interp2 = 1
    # Case2: value1 NOT in data1; num_interp1 = 1; look for value2 in both data2[:,idx] and data2[:,idx+1]   
    else:
        num_interp1 = 1
        # Case2_1: value2 in both data2[:,idx] and data2[:,idx+1]; num_interp2 = 0
        if value2 in data2[:,idx] and value2 in data2[:,idx+1]:
            num_interp2 = 0
        # Case2_2: value2 in data2[:,idx]; num_interp2 = 1
        elif value2 in data2[:,idx]:
            num_interp2 = 1
        # Case2_3: value2 in data2[:,idx+1]; num_interp2 = 2
        elif value2 in data2[:, idx+1]:
            num_interp2 = 2
        # Case2_4: value2 NOT in data2[:,idx] or data2[:,idx+1]; num_interp2 = 3
        else:
            num_interp2 = 3
    jdx = proper_index(jdx,idx,data2,value2)
    kdx = proper_index(kdx,idx+1,data2,value2)
    return idx, jdx, kdx, num_interp1, num_interp2


# interp: Int Int Int Int Int Int numpy.ndarray numpy.ndarray numpy.ndarray Num Num -> Num
def interp(idx, jdx, kdx, num_interp1, num_interp2, data1, data2, data3, value1, value2):
    """
    Performs the appropriate number of interpolations based on the output of find_bounds.
    """
    # Case1_1: value1 in data1 and value2 in data2[:,idx]; no need for interpolation, interp_value = data3[jdx,idx]
    if num_interp1 == 0 and num_interp2 == 0:
        interp_value = data3[jdx, idx]
    # Case1_2: value1 in data1 but value2 NOT in data2[:,idx]; use the closest values above and below value2 in data2[:,idx] to do linear interpolation, 
    #   where (x1,y1)=(data2[jdx,idx],data3[jdx,idx]), (x2,y2)=(data2[jdx+1,idx],data3[jdx+1,idx])
    elif num_interp1 == 0 and num_interp2 == 1:
        interp_value = lin_interp(data3[jdx, idx], data3[jdx+1, idx], data2[jdx, idx], data2[jdx+1, idx], value2)
    # Case2_1: value1 NOT in data1 but value2 in both data2[:,idx] and data2[:,idx+1]; use the closest values above and below value1 to do linear
    #   interpolation, where (x1,y1)=(data1[idx],data3[jdx,idx]), (x2,y2)=(data1[idx+1],data3[kdx,idx+1]) 
    elif num_interp1 == 1 and num_interp2 == 0:
        y0_1 = data3[jdx, idx]
        y0_2 = data3[kdx, idx+1]
        interp_value = lin_interp(y0_1, y0_2, data1[idx], data1[idx+1], value1)
    # Case2_2: value1 NOT in data1 but value2 in data2[:,idx]; use the closest values below and above value2 in data2[:,idx+1] to do linear interpolation for 
    #   y2(x2=data1[idx+1] but value2 not in data2[:,idx+1]); then use the closest values below and above value1 to do linear interpolation for interp_value, 
    #   where (x1,y1)=(data1[idx],data3[jdx,idx]), (x2,y2)=(data1[idx+1],lin_interp(data3[kdx,idx+1],data3[kdx+1,idx+1],data2[kdx,idx+1],data2[kdx+1,idx+1],value2))
    elif num_interp1 == 1 and num_interp2 == 1:
        y0_1 = data3[jdx, idx]
        y0_2 = lin_interp(data3[kdx, idx+1], data3[kdx+1, idx+1], data2[kdx, idx+1], data2[kdx+1, idx+1], value2)
        interp_value = lin_interp(y0_1, y0_2, data1[idx], data1[idx+1], value1)
    # Case2_3: value1 NOT in data1 but value2 in data2[:,idx+1]; use the closest values below and above value2 in data2[:,idx] to do linear interpolation for
    #   y1(x1=data1[idx] but value2 not in data2[:,idx]); then use the closest values below and above value1 to do linear interpolation for interp_value, where
    #   (x1,y1)=(data1[idx],lin_interp(data3[jdx,idx],data3[jdx+1,idx],data2[jdx,idx],data2[jdx+1.idx],value2)), (x2,y2)=(data1[idx+1], data3[kdx,idx+1])
    elif num_interp1 == 1 and num_interp2 ==2:
        y0_2 = data3[kdx, idx+1]
        y0_1 = lin_interp(data3[jdx, idx], data3[jdx+1, idx], data2[jdx, idx], data2[jdx+1, idx], value2)
        x1_1 = data1[idx]
        x1_2 = data1[idx+1]
        interp_value = lin_interp(y0_1, y0_2, x1_1, x1_2, value1)
    # Case2_4: value1 NOT in data1 and value2 NOT in data2[:,idx] or data2[:,idx+1]; use the closest values below and above value2 in data2[:,idx] to do 
    #   linear interpolation for y1 and the closest values below and above value2 in data2[:,idx+1] to do linear interpolation for y2; then use the closest
    #   values of value1 in data1 to do linear interpolation for interp_value, where (x1,y1)=(data1[idx],lin_interp(data3[jdx,idx],data3[jdx+1,idx],
    #   data2[jdx,idx],data2[jdx+1,idx],value2)), (x2,y2)=(data1[idx+1],lin_interp(data3[kdx,idx+1],data3[kdx+1,idx+1],data2[kdx,idx+1],data2[kdx+1,idx+1],value2))
    elif num_interp1 == 1 and num_interp2 == 3:
        y0_1 = lin_interp(data3[jdx, idx], data3[jdx+1, idx], data2[jdx, idx], data2[jdx+1, idx], value2)
        y0_2 = lin_interp(data3[kdx, idx+1], data3[kdx+1, idx+1], data2[kdx, idx+1], data2[kdx+1, idx+1], value2)
        x0_1 = data1[idx]
        x0_2 = data1[idx+1]
        interp_value = lin_interp(y0_1, y0_2, x0_1, x0_2, value1)
    return interp_value