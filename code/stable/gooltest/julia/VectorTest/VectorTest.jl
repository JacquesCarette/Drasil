module VectorTest

v1 = [1.0, 1.5]
v2 = [0.0, -1.0]
for i in 0:1:length(v1) - 1
    v1[1] = 2.0 * v1[1] + v2[1]
end
x = 0.0
for j in 0:1:length(v1) - 1
    global x += v1[1] * v2[1];
end
@assert x == -2.0 "Dot product of v1 and v2 should be -2."

end
