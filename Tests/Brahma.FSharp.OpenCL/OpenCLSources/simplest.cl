__kernel void simplest(__global int* a, const int val) {
    const int id = get_global_id(0);
    a[id] = val;
}
