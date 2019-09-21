
template <typename T>
struct arr10_t {
    int currIndex = 0;
    T arr[10];

    HOST DEV T operator [] (int i) const {return arr[i];}
    HOST DEV T& operator [] (int i) {return arr[i];}

    HOST DEV void push_back(T obj) {
        arr[currIndex] = obj;
        currIndex++;
    }

    HOST DEV int size() {
        return currIndex;
    }

    HOST DEV T* begin() {
        return arr;
    }

    HOST DEV T* end() {
        return &arr[currIndex];
    }
};
