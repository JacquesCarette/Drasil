var v1: [Double] = [1.0, 1.5]
var v2: [Double] = [0.0, -1.0]
for i in [Int](stride(from: 0, to: v1.count, by: 1)) {
    v1[i] = 2.0 * v1[i] + v2[i]
}
var x: Double
x = 0.0
for j in [Int](stride(from: 0, to: v1.count, by: 1)) {
    x += v1[j] * v2[j];
}
