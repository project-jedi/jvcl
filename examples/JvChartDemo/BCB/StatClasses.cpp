//---------------------------------------------------------------------------


#pragma hdrstop

#include "StatClasses.h"
#include <Math.hpp>

//---------------------------------------------------------------------------

#pragma package(smart_init)
#pragma link "Math"

TStatArray::TStatArray()
{
   //FFirst = true;
   FLength = 0;
   FIndex = 0;
   FCount = 0;
   FGrows = true;
}

void TStatArray::Reset()
{
    FIndex = 0;
    FCount = 0;
    for(int i = 0; i < FLength; i++)
    {
      FValues[i] = -999.0; // debug helper.
    }
    //FFirst = true;
}

TStatArray::TStatArray(int initialLength)
{
//   FFirst = true;
  FValues.reserve(initialLength);
   if (initialLength>0)
      FGrows = false;
   else
      FGrows = true;

   FLength = initialLength;
   FIndex = 0;
   FCount = 0;
}


TStatArray::~TStatArray()
{
}


double TStatArray::Average()
{
 int last,i;
 double sum;

 if (FCount <= 0)
 {
    return 0;
 }
 else
 {
    sum = 0;
    if (FCount>FLength)
        last = FLength-1;
    else
        last  =FCount-1;

    for(i = 0; i <= last; i ++)
    {
       sum = sum + FValues[i];
    }
    return sum / (last+1);
 }
}

double TStatArray::StandardDeviation()
{
// TempArray:Array of Double;

  if (FCount <= 0)
  {
      return 0;
  }
  else
  {
    double* tmp = new double[FCount];
    for(int i = 0; i < FCount; i++)
        tmp[i] = FValues[i];
    double Result = Math::StdDev(tmp, FCount);
    delete [] tmp;
    return Result;
  }
}

void TStatArray::AddValue(double aValue)
{
 // First time in we might need to create an array:
  if (FIndex >= Length)
  {
//     assert(FGrows); // Illegal condition.
     FLength = FIndex+1;
     FValues.reserve(FLength); // uninitialized, as of yet.
  }

  FValues[FIndex] = aValue;
  FIndex++;
  FCount++;

  if (!FGrows) // circular?
  {
    if (FIndex >= FLength)
    {
        FIndex = 0;
        //FCount = FLength;//FCount does not exceed FLength in wraparounds.
    }
  }
  else // grow after, in doublings of size, scales better!
  {
    if (FIndex >= FLength)
    {
      FLength = FLength * 2;
      FValues.reserve(FLength); // uninitialized, as of yet.
    }
  }
}

void TStatArray::SetLen(int aLength)
{
  if (aLength<1)
      aLength = 1;

  FLength = aLength;
  FValues.reserve(FLength);
}

