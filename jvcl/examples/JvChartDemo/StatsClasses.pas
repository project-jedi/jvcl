{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChartDemoFm.Pas, released on 2002-10-04.

The Initial Developer of the Original Code is AABsoft and Mårten Henrichson.
(C) 1996 AABsoft and Mårten Henrichson.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-07
  Modified 2003 Warren Postma

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  TStatArray - Statistics, Rolling average helper class.

Known Issues:
  This version is part of JvChartDemo.
-----------------------------------------------------------------------------}

unit StatsClasses;


interface


type

TStatArray = class
  protected
   //FFirst:Boolean;
   FGrows:Boolean; // false=rolling average (circular buffer mode), true=average or sd of any number of samples (array grows without limit)
   FValues:Array of Double;
   FLength, // Array absolute size (may still be no data even if this is >0)
   FIndex,  // Where will the next sample be stored into the array?
   FCount:  // How many valid samples are in the array right now?
      Integer;


   procedure SetLen(aLength:Integer);


  public
    procedure AddValue(aValue:Double);

    function Average:Double;

    function StandardDeviation:Double;

    property Grows:Boolean read FGrows write FGrows; // false=rolling average, true=average ALL samples (grows to fit)

    property Length:Integer read FLength write SetLen;
    property Count:Integer read FCount;
    //property First:Boolean read FFirst;

    procedure Reset; // Clear everything!

    constructor Create; overload;
    constructor Create(initialLength:Integer); overload;
    destructor Destroy; override;
end;

implementation

uses  Windows, // OutputDebugString
      SysUtils, // FloatToStr
      Math;  //  VCL's statistics routines. StdDev, etc.

// Begin Rolling Average

constructor  TStatArray.Create; // overload;
begin
   //FFirst := true;
   FLength := 0;
   FIndex := 0;
   FCount:= 0;
   FGrows := true;
   SetLength(FValues,0);   
end;

procedure TStatArray.Reset;
var
 i:Integer;
begin
    FIndex := 0;
    FCount := 0;
    for i := 0 to FLength-1 do begin
      FValues[i] := -999.0; // debug helper.
    end;
    //FFirst := true;
end;

constructor TStatArray.Create(initialLength:Integer); // overload;
begin
//   FFirst := true;
   SetLength(FValues,initialLength);
   if (initialLength>0) then
      FGrows := false
   else
      FGrows := true; 
   FLength := initialLength;
   FIndex := 0;
   FCount:= 0;
end;


destructor   TStatArray.Destroy;
begin
  SetLength(FValues,0);
end;


function     TStatArray.Average:Double;
var
 last,i:Integer;
 sum:Double;
begin
 if FCount <= 0 then begin
    result := 0;
 end else begin
    sum := 0;
    if (FCount>FLength) then
        last :=FLength-1
    else
        last :=FCount-1;
    for i := 0 to last do begin
       sum := sum + FValues[i];
    end;
    result := sum / (last+1);
 end;
end;

function TStatArray.StandardDeviation:Double;
var
 i:Integer;
// sum:Double;
 TempArray:Array of Double;
begin
  if (FCount <= 0) then
      result := 0
  else if (FCount >= FLength) then begin
      result := Math.StdDev( FValues )
  end else begin
      SetLength(TempArray,FCount);
      for i := 0 to FCount-1 do begin
          TempArray[i] := FValues[i];
      end;
      result := Math.StdDev( TempArray );
      SetLength(TempArray,0);
  end;
end;

procedure    TStatArray.AddValue(aValue:Double);
//var
// i:Integer;
begin
 (*if FFirst then begin
    FFirst := false;
    FIndex := 0;
    FValues[0] := aValue;
    FCount := 1;
 end else begin*)

 // First time in we might need to create an array:
  if FIndex>=Length then begin
     Assert(FGrows); // Illegal condition.
     FLength := FIndex+1;
     SetLength( FValues,FLength); // uninitialized, as of yet.
  end;

  FValues[FIndex] := aValue;
  Inc(FIndex);
  Inc(FCount);
  if (not FGrows) then begin // circular?
    if (FIndex>=FLength) then begin
        FIndex := 0;
        //FCount := FLength;//FCount does not exceed FLength in wraparounds.
    end;
  end else begin // grow after, in doublings of size, scales better!
    if (FIndex>=FLength) then begin
      FLength := FLength * 2;
      SetLength( FValues,FLength); // uninitialized, as of yet.
{$ifdef DEBUG_ASSERTIONS}  
      Assert(FLength<20000); // Debug limit
{$endif}      
    end;
  end;

end;

procedure TStatArray.SetLen(aLength:Integer);
begin
  if(aLength<1) then
      aLength := 1;
  FLength := aLength;
  SetLength(FValues, FLength);

end;


// End Stats


{$ifdef UNIT_TESTS}
procedure StatsUnitTests;
var
   diff:Double;
    a1:TStatArray;
    procedure _outs(s:String);
    begin
        OutputDebugString(PChar(s));
    end;
    procedure _outd(d:Double);
    begin
        OutputDebugString(PChar(FloatToStr(d)));
    end;
    
begin
   _outs('StatsUnitTests begins');

   a1 := TStatArray.Create(0); // Growing array.
   a1.AddValue( 3.5 );
   a1.AddValue( 1.5 );
   a1.AddValue( 25.5 );
   a1.AddValue( 100.5 );
   _outd( a1.Average );

   diff := Abs(((3.5+1.5+25.5+100.5)/4.0)-a1.Average);
   Assert(diff<0.001);

   a1.Reset;
   Assert(Abs(a1.Average)<0.0001);

   a1.AddValue( 3.5 );
   a1.AddValue( 1.5 );
   a1.AddValue( 25.5 );
   a1.AddValue( 100.5 );
   _outd( a1.Average );

   diff := Abs(((3.5+1.5+25.5+100.5)/4.0)-a1.Average);
   Assert(diff<0.001);

   _outd( a1.StandardDeviation );
   Assert(trunc(a1.StandardDeviation)=46);

   _outs('StatsUnitTests ends');   
end;

initialization
    StatsUnitTests;

{$endif}


end.
