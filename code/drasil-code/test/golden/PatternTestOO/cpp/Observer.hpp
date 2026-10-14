#ifndef Observer_h
#define Observer_h

/** \brief This is an arbitrary class acting as an Observer
*/
class Observer {
    public:
        int x;
        
        Observer();
        void printNum();
        int getX();
        void setX(int x);
};

#endif
