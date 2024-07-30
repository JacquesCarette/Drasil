module VectorTest

v1 = [1.0, 1.5]
v2 = [0.0, -1.0]
for i in 0:1:length(v1) - 1
    v1[i + 1] = 2.0 * v1[i + 1] + v2[i + 1]
end
x = 0.0
for j in 0:1:length(v1) - 1
    x += v1[j + 1] * v2[j + 1];
end

end
