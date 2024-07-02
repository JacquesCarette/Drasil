v1 = [1.0, 1.5]
v2 = [0.0, -1.0]
for i in range(0, len(v1), 1):
    v1[i] = 2.0 * v1[i] + v2[i]
x = 0.0
for j in range(0, len(v1), 1):
    x += v1[j] * v2[j];
