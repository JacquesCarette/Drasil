#include <vector>

using std::vector;

int main(int argc, const char *argv[]) {
    vector<double> v1{1.0, 1.5};
    vector<double> v2{0.0, -1.0};
    for (int i = 0; i < (int)(v1.size()); i += 1) {
        v1.at(i) = 2.0 * v1.at(i) + v2.at(i);
    }
    double x;
    x = 0.0;
    for (int j = 0; j < (int)(v1.size()); j += 1) {
        x += v1.at(j) * v2.at(j);
    }
    
    return 0;
}
