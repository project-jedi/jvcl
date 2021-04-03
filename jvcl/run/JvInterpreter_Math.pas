{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Math.pas, released on 2005-08-15.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):  Peter Schraut (http://www.console-dev.de)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description: JvInterpreter_Math adds most functions from math.pas to
             to JvInterpreter. Functions in this file are
             sorted as they appear in the Delphi 6 helpfile, same applies
             to functionregistration in RegisterJvInterpreterAdapter.
             Missing functions are marked with a "TODO: add xxx function". If
             you add a new function, remove the comment.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{ history (JVCL Library versions):

Upcoming JVCL 3.00
    - initial version
}

unit JvInterpreter_Math;

{$I jvcl.inc}

interface

uses
  Variants,
  JvInterpreter, SysUtils;


procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);


implementation

uses
  Math;

procedure JvInterpreter_ArcCos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcCos(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcCosh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcCosh(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcCot(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcCot(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcCotH(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcCotH(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcCsc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcCsc(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcCscH(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcCscH(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcSec(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcSec(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcSecH(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcSecH(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcSin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcSin(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcSinh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcSinh(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ArcTan2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcTan2(Extended(Args.Values[0]), Extended(Args.Values[1]));
end;

procedure JvInterpreter_ArcTanh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.ArcTanh(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Ceil(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Ceil(Extended(Args.Values[0]));
end;

procedure JvInterpreter_ClearExceptions(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Math.ClearExceptions;
end;

procedure JvInterpreter_Cosecant(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Cosecant(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Cosh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Cosh(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Cot(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Cot(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Cotan(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Cotan(Extended(Args.Values[0]));
end;

procedure JvInterpreter_CotH(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.CotH(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Csc(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Csc(Extended(Args.Values[0]));
end;

procedure JvInterpreter_CscH(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.CscH(Extended(Args.Values[0]));
end;

procedure JvInterpreter_CycleToDeg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.CycleToDeg(Extended(Args.Values[0]));
end;

procedure JvInterpreter_CycleToGrad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.CycleToGrad(Extended(Args.Values[0]));
end;

procedure JvInterpreter_CycleToRad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.CycleToRad(Extended(Args.Values[0]));
end;

procedure JvInterpreter_DegToCycle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.DegToCycle(Extended(Args.Values[0]));
end;

procedure JvInterpreter_DegToGrad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.DegToGrad(Extended(Args.Values[0]));
end;

procedure JvInterpreter_DegToRad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.DegToRad(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Floor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Floor(Extended(Args.Values[0]));
end;

procedure JvInterpreter_GradToCycle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.GradToCycle(Extended(Args.Values[0]));
end;

procedure JvInterpreter_GradToDeg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.GradToDeg(Extended(Args.Values[0]));
end;

procedure JvInterpreter_GradToRad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.GradToRad(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Hypot(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Hypot(Extended(Args.Values[0]), Extended(Args.Values[1]));
end;

procedure JvInterpreter_IntPower(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.IntPower(Extended(Args.Values[0]), Integer(Args.Values[1])));
end;

procedure JvInterpreter_IsInfinite(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Boolean(Math.IsInfinite(Double(Args.Values[0])));
end;

procedure JvInterpreter_IsNan(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Boolean(Math.IsNan(Double(Args.Values[0])));
end;

procedure JvInterpreter_Ldexp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.Ldexp(Extended(Args.Values[0]), Integer(Args.Values[1])));
end;

procedure JvInterpreter_LnXP1(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.LnXP1(Extended(Args.Values[0])));
end;

procedure JvInterpreter_Log10(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.Log10(Extended(Args.Values[0])));
end;

procedure JvInterpreter_Log2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.Log2(Extended(Args.Values[0])));
end;

procedure JvInterpreter_LogN(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.LogN(Extended(Args.Values[0]), Extended(Args.Values[1])));
end;

procedure JvInterpreter_Max(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Max(Integer(Args.Values[0]), Integer(Args.Values[1]));
end;

procedure JvInterpreter_Min(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Min(Integer(Args.Values[0]), Integer(Args.Values[1]));
end;

procedure JvInterpreter_Power(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.Power(Extended(Args.Values[0]), Extended(Args.Values[1])));
end;

procedure JvInterpreter_RadToCycle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.RadToCycle(Extended(Args.Values[0])));
end;

procedure JvInterpreter_RadToDeg(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.RadToDeg(Extended(Args.Values[0])));
end;

procedure JvInterpreter_RadToGrad(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.RadToGrad(Extended(Args.Values[0])));
end;

procedure JvInterpreter_RandG(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Extended(Math.RandG(Extended(Args.Values[0]), Extended(Args.Values[1])));
end;

procedure JvInterpreter_RandomRange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(Math.RandomRange(Integer(Args.Values[0]), Integer(Args.Values[1])));
end;

procedure JvInterpreter_Sec(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Sec(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Secant(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Secant(Extended(Args.Values[0]));
end;

procedure JvInterpreter_SecH(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.SecH(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Sinh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Sinh(Extended(Args.Values[0]));
end;

procedure JvInterpreter_SLNDepreciation(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.SLNDepreciation(Extended(Args.Values[0]), Extended(Args.Values[1]), Integer(Args.Values[2]));
end;

procedure JvInterpreter_SYDDepreciation(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.SYDDepreciation(Extended(Args.Values[0]), Extended(Args.Values[1]), Integer(Args.Values[2]), Integer(Args.Values[3]));
end;

procedure JvInterpreter_Tan(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Tan(Extended(Args.Values[0]));
end;

procedure JvInterpreter_Tanh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Math.Tanh(Extended(Args.Values[0]));
end;


{ RegisterJvInterpreterAdapter }
procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cMath = 'Math';
  // MinExtended is platform specific in Delphi 10.2 and newer This prevents the
  // platform warning without playing with the "$WARN PLAFORM_SYMBOL" compiler option.
  {$IF declared(MinExtended80)}
    {$IF SizeOf(Extended) >= 10}
  MinExtendedValue = MinExtended80;
    {$ELSE}
  MinExtendedValue = MinDouble; // SSE instruction set doesn't support 10-Byte Extended
    {$IFEND}
  {$ELSE}
  MinExtendedValue = MinExtended;
  {$IFEND}
begin
  with JvInterpreterAdapter do
  begin
    // add constants
    AddConst(cMath, 'PI', PI);
    AddConst(cMath, 'Infinity', Infinity);
    AddConst(cMath, 'MaxComp', MaxComp);
    AddConst(cMath, 'MaxDouble', MaxDouble);
    // TODO: fix MaxExtended, raises and overflow error atm
    //AddConst(cMath, 'MaxExtended', MaxExtended);
    AddConst(cMath, 'MaxSingle', MaxSingle);
    AddConst(cMath, 'MinComp', MinComp);
    AddConst(cMath, 'MinDouble', MinDouble);
    AddConst(cMath, 'MinExtended', MinExtendedValue);
    AddConst(cMath, 'MinSingle', MinSingle);
    AddConst(cMath, 'NaN', NaN);
    AddConst(cMath, 'NegInfinity', NegInfinity);

    // add functions
    AddFunction(cMath, 'ArcCos', JvInterpreter_ArcCos, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcCosh', JvInterpreter_ArcCosh, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcCot', JvInterpreter_ArcCot, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcCotH', JvInterpreter_ArcCot, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcCsc', JvInterpreter_ArcCsc, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcCscH', JvInterpreter_ArcCsc, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcSec', JvInterpreter_ArcSec, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcSecH', JvInterpreter_ArcSecH, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcSin', JvInterpreter_ArcSin, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcSinh', JvInterpreter_ArcSinh, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ArcTan2', JvInterpreter_ArcTan2, 2, [varEmpty,varEmpty], varEmpty);
    AddFunction(cMath, 'ArcTanh', JvInterpreter_ArcTanh, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Ceil', JvInterpreter_Ceil, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'ClearExceptions', JvInterpreter_ClearExceptions, 0, [], varEmpty);
    // TODO: add CompareValue function
    AddFunction(cMath, 'Cosecant', JvInterpreter_Cosecant, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Cosh', JvInterpreter_Cosh, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Cot', JvInterpreter_Cot, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Cotan', JvInterpreter_Cotan, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'CotH', JvInterpreter_CotH, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Csc', JvInterpreter_Csc, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'CscH', JvInterpreter_Csc, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'CycleToDeg', JvInterpreter_CycleToDeg, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'CycleToGrad', JvInterpreter_CycleToGrad, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'CycleToRad', JvInterpreter_CycleToRad, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'DegToCycle', JvInterpreter_DegToCycle, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'DegToGrad', JvInterpreter_DegToGrad, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'DegToRad', JvInterpreter_DegToRad, 1, [varEmpty], varEmpty);
    // TODO: add DivMod function
    // TODO: add DoubleDecliningBalance function
    // TODO: add EnsureRange function
    AddFunction(cMath, 'Floor', JvInterpreter_Floor, 1, [varEmpty], varEmpty);
    // TODO: add Frexp procedure
    // TODO: add FutureValue function
    // TODO: add GetExceptionMask function
    // TODO: add GetPrecisionMode function
    // TODO: add GetRoundMode function
    AddFunction(cMath, 'GradToCycle', JvInterpreter_GradToCycle, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'GradToDeg', JvInterpreter_GradToDeg, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'GradToRad', JvInterpreter_GradToRad, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Hypot', JvInterpreter_Hypot, 2, [varEmpty,varEmpty], varEmpty);
    // TODO: add InRange function
    // TODO: add InterestPayment function
    // TODO: add  InterestRate function
    // TODO: add  InternalRateOfReturn function
    AddFunction(cMath, 'IntPower', JvInterpreter_IntPower, 2, [varEmpty,varEmpty], varEmpty);
    AddFunction(cMath, 'IsInfinite', JvInterpreter_IsInfinite, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'IsNan', JvInterpreter_IsNan, 1, [varEmpty], varEmpty);
    // TODO: add IsZero function
    AddFunction(cMath, 'Ldexp', JvInterpreter_Ldexp, 2, [varEmpty,varEmpty], varEmpty);
    AddFunction(cMath, 'LnXP1', JvInterpreter_LnXP1, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Log10', JvInterpreter_Log10, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Log2', JvInterpreter_Log2, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'LogN', JvInterpreter_LogN, 2, [varEmpty,varEmpty], varEmpty);
    AddFunction(cMath, 'Max', JvInterpreter_Max, 2, [varEmpty, varEmpty], varEmpty);
    // TODO: add MaxIntValue function
    // TODO: add MaxValue function
    // TODO: add Mean function
    AddFunction(cMath, 'Min', JvInterpreter_Min, 2, [varEmpty, varEmpty], varEmpty);
    // TODO: add MinIntValue function
    // TODO: add MinValue function
    // TODO: add MomentSkewKurtosis function
    // TODO: add NetPresentValue function
    // TODO: add Norm function
    // TODO: add NumberOfPeriods function
    // TODO: add Payment function
    // TODO: add PeriodPayment function
    // TODO: add Poly function
    // TODO: add PopnStdDev function
    // TODO: add PopnVariance function
    AddFunction(cMath, 'Power', JvInterpreter_Power, 2, [varEmpty,varEmpty], varEmpty);
    // TODO: add PresentValue function
    AddFunction(cMath, 'RadToCycle', JvInterpreter_RadToCycle, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'RadToDeg', JvInterpreter_RadToDeg, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'RadToGrad', JvInterpreter_RadToGrad, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'RandG', JvInterpreter_RandG, 2, [varEmpty,varEmpty], varEmpty);
    AddFunction(cMath, 'RandomRange', JvInterpreter_RandomRange, 2, [varEmpty,varEmpty], varEmpty);
    // TODO: add RoundTo function
    // TODO: add SameValue function
    AddFunction(cMath, 'Sec', JvInterpreter_Sec, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Secant', JvInterpreter_Secant, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'SecH', JvInterpreter_SecH, 1, [varEmpty], varEmpty);
    // TODO: add SetExceptionMask function
    // TODO: add SetPrecisionMode function
    // TODO: add SetRoundMode function
    // TODO: add Sign function
    // TODO: add SimpleRoundTo function
    // TODO: add SinCos function
    AddFunction(cMath, 'Sinh', JvInterpreter_Sinh, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'SLNDepreciation', JvInterpreter_SLNDepreciation, 3, [varEmpty,varEmpty,varEmpty], varEmpty);
    // TODO: add StdDev function
    // TODO: add Sum function
    // TODO: add SumInt function
    // TODO: add SumOfSquares function
    // TODO: add SumsAndSquares function
    AddFunction(cMath, 'SYDDepreciation', JvInterpreter_SYDDepreciation, 4, [varEmpty,varEmpty,varEmpty,varEmpty], varEmpty);
    AddFunction(cMath, 'Tan', JvInterpreter_Tan, 1, [varEmpty], varEmpty);
    AddFunction(cMath, 'Tanh', JvInterpreter_Tanh, 1, [varEmpty], varEmpty);
    // TODO: add TotalVariance function
    // TODO: add Variance function
  end;
end;


end.

