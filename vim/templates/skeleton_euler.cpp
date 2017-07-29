/* Template for Project Euler problems.
 * Problem header goes here.*/

#include <iostream>
using namespace std;
#include <sys/time.h>
typedef unsigned long long timestamp_t;

static timestamp_t
get_timestamp (){
    struct timeval now;
    gettimeofday (&now, NULL);
    return  now.tv_usec + (timestamp_t)now.tv_sec * 1000000;
}

    void compute(){
        // Problem calculations go here.
    }

    int main(int argc, const char* argv[]){
        // This is where the code will be benchmarked.
        timestamp_t t0 = get_timestamp();
        compute();
        timestamp_t t1 = get_timestamp();

        double secs = (t1 - t0) / 1000000.0L;
        cout << "Time elapsed: " << secs << " seconds.\n";
        return 0;
    }
