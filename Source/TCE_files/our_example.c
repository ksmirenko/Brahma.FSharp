int func (int x, int y) {
    
    int r = 0;
    if (x < y) r = func (x + 2, y - 1);
    else r = x + y;
    return r;
}

int result;

int main (void) {        
    
    result = func (2, 10);
    return 0;
}
