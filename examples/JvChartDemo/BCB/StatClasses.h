//---------------------------------------------------------------------------

#ifndef StatClassesH
#define StatClassesH

#include <vector>
using namespace std;

class TStatArray
{
protected:
   //bool FFirst;
   bool FGrows; // false=rolling average (circular buffer mode), true=average or sd of any number of samples (array grows without limit)
   vector<bool> FValues;
   int FLength; // Array absolute size (may still be no data even if this is >0)
   int FIndex;  // Where will the next sample be stored into the array?
   int FCount;  // How many valid samples are in the array right now?


   void SetLen(int aLength);
   
public:
    void AddValue(double aValue);

    double Average();

    double StandardDeviation();

    __property bool Grows = {read=FGrows, write=FGrows}; // false=rolling average, true=average ALL samples (grows to fit)

    __property int Length = {read=FLength, write=SetLen};
    __property int Count = {read=FCount};
    //__property bool First = {read=FFirst};

    void Reset(); // Clear everything!

    TStatArray();
    TStatArray(int initialLength);
    ~TStatArray();
};

//---------------------------------------------------------------------------
#endif
