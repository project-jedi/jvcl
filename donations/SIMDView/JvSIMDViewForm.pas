{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDViewForm.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSIMDViewForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolsApi, Grids, ExtCtrls, Menus, JvSIMDUtils;

type
  TSIMDDisplay = ( sdBytes, sdWords, sdDWords, sdQWords, sdSingles, sdDoubles );
  TSIMDFormat = ( sfBinary, sfSigned, sfUnsigned, sfHexa );

  TSSEForm = class(TForm)
    Splitter: TSplitter;
    ListBoxRegs: TListBox;            // ListBoxRegs.Items.Objetcs         Bit 0 : Changed
    ListBoxMXCSR: TListBox;           // ListBoxMXCSR.Items.Objects        Bits 0 to 7 : BitId (TJvMXCSRBits)
                                      //                                   Bit 8 : Changed
    PopupMenuRegs: TPopupMenu;
    PopupMenuMXCSR: TPopupMenu;
    MenuItemComplement: TMenuItem;
    MenuItemBinary: TMenuItem;
    MenuItemSigned: TMenuItem;
    MenuItemUnsigned: TMenuItem;
    MenuItemHexa: TMenuItem;
    MenuItemDisplay: TMenuItem;
    MenuItemFormat: TMenuItem;
    MenuItemBytes: TMenuItem;
    MenuItemWords: TMenuItem;
    MenuItemDWords: TMenuItem;
    MenuItemQWords: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSingles: TMenuItem;
    MenuItemDoubles: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MenuItemStayOnTop: TMenuItem;
    procedure ListBoxMXCSRDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMXCSRMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMXCSRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemComplementClick(Sender: TObject);
    procedure MenuItemFormatClick(Sender: TObject);
    procedure MenuItemDisplayClick(Sender: TObject);
    procedure MenuItemStayOnTopClick(Sender: TObject);
  private
    FServices:IOTADebuggerServices;
    FVectorFrame:TJvVectorFrame;
    FDisplay:TSIMDDisplay;
    FFormat:TSIMDFormat;
    function FormatBinary(Value: Int64; nbDigit: Byte): string;
    function FormatHexa(Value: Int64; nbDigit: Byte): string;
    function FormatSigned(Value: Int64; nbDigit: Byte): string;
    function FormatUnsigned(Value: Int64; nbDigit: Byte): string;
    procedure SetDisplay(const Value: TSIMDDisplay);
    procedure SetFormat(const Value: TSIMDFormat);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create (AOwner:TComponent;
     AServices:IOTADebuggerServices); reintroduce;
    destructor Destroy; override;
    procedure SetThreadValues;
    procedure GetThreadValues;
    procedure FormatRegs;
    property Format:TSIMDFormat read FFormat write SetFormat;
    property Display:TSIMDDisplay read FDisplay write SetDisplay;
  end;

var
  SSEForm: TSSEForm;

implementation

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{$R *.dfm}

constructor TSSEForm.Create(AOwner: TComponent;
  AServices: IOTADebuggerServices);
var
  I:TJvMXCSRBits;
  J:Integer;
begin
  inherited Create(AOwner);
  FServices:=AServices;
  ListBoxMXCSR.Items.Clear;
  for I:=mbInvalidOperationException to mbFlushToZero do
    if MXCSRBitsDescription[I].BitCount<>0 then
      ListBoxMXCSR.Items.AddObject('0',TObject(I));
  ListBoxRegs.Items.Clear;
  if (ProcessorVendorID=viAMD) and (asLong in AMDSIMD) then
    J := 16
  else
    J := 8;
  while (J>-1) do   // 16 128-bit registers + 1 cardinal (MXCSR)
  begin
    ListBoxRegs.Items.InsertObject(0,'0',nil);
    Dec(J);
  end;
  MenuItemBinary.Tag:=Integer(sfBinary);
  MenuItemSigned.Tag:=Integer(sfSigned);
  MenuItemUnsigned.Tag:=Integer(sfUnsigned);
  MenuItemHexa.Tag:=Integer(sfHexa);
  MenuItemBytes.Tag:=Integer(sdBytes);
  MenuItemWords.Tag:=Integer(sdWords);
  MenuItemDWords.Tag:=Integer(sdDWords);
  MenuItemQWords.Tag:=Integer(sdQWords);
  MenuItemSingles.Tag:=Integer(sdSingles);
  MenuItemDoubles.Tag:=Integer(sdDoubles);
  Format:=sfHexa;
  Display:=sdWords;
  GetThreadValues;
end;

destructor TSSEForm.Destroy;
begin
  inherited Destroy;
end;

procedure TSSEForm.ListBoxMXCSRDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  BitId:TJvMXCSRBits;
begin
  with (Control as TListBox),Canvas do
  begin
    if not (odFocused in State) then
    begin
      Pen.Color:=Brush.Color;
      if (odSelected in State)
        then Font.Color:=clWindow;
    end;
    Rectangle(Rect);
    BitId:=TJvMXCSRBits(Cardinal(Items.Objects[Index]) and $FF);
    TextOut(Rect.Left+2,Rect.Top,MXCSRBitsDescription[BitId].ShortName);
    if ((Cardinal(Items.Objects[Index]) and $100)<>0)
      then Font.Color:=clRed;
    TextOut(Rect.Left+2+TextExtent(MXCSRBitsDescription[BitId].ShortName).cx,Rect.Top,
            Items[Index]);
  end;
end;

procedure TSSEForm.ListBoxMXCSRMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  AIndex:Integer;
  AText:string;
  BitId:TJvMXCSRBits;
begin
  if (Shift<>[])
    then Application.HideHint
    else with (Sender as TListBox) do
  begin
    AIndex:=ItemAtPos(Point(X,Y),True);
    if (AIndex>=0) and (AIndex<Items.Count) then
    begin
      BitId:=TJvMXCSRBits(Cardinal(Items.Objects[AIndex]) and $FF);
      with MXCSRBitsDescription[BitId] do
      begin
        AText := LongName;
        if BitCount=2 then
          AText := SysUtils.Format('%s (%s)',[AText,Names[(Cardinal(FVectorFrame.MXCSR) shr 13) and 3]]);
        if (AText<>Hint) then
        begin
          Hint:=AText;
          Application.HideHint;
          Application.ActivateHint(Point(X,Y));
        end;
      end;
    end
    else
    begin
      Hint:='';
      Application.HideHint;
    end;
  end;
end;

procedure TSSEForm.ListBoxRegsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  AText:string;
begin
  with (Control as TListBox),Canvas do
  begin
    if not (odFocused in State) then
    begin
      Pen.Color:=Brush.Color;
      if (odSelected in State)
        then Font.Color:=clWindow;
    end;
    Rectangle(Rect);
    if Index>0 then
      AText:='XMM'+SysUtils.Format('%.2d',[Index-1])
    else
      AText:='MXCSR ';
    TextOut(Rect.Left+2,Rect.Top,AText);
    if Boolean(Items.Objects[Index])
      then Font.Color:=clRed;
    TextOut(Rect.Left+2+TextExtent(AText).cx,Rect.Top,
            Items[Index]);
  end;
end;

procedure TSSEForm.ListBoxMXCSRMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIndex:Integer;
begin
  if (Button=mbRight) then
    with (Sender as TListBox) do
  begin
    AIndex:=ItemAtPos(Point(X,Y),True);
    if (AIndex>-1) then
    begin
      ItemIndex:=AIndex;
      with ClientToScreen(Point(X,Y)) do
        PopupMenu.Popup(X,Y);
    end;
  end;
end;

procedure TSSEForm.MenuItemComplementClick(Sender: TObject);
var
  AndMask:Cardinal;
  BitValue:Cardinal;
  BitId:TJvMXCSRBits;
begin
  with ListBoxMXCSR do
  begin
    BitId:=TJvMXCSRBits(Cardinal(Items.Objects[ItemIndex]) and $FF);
    with MXCSRBitsDescription[BitId], FVectorFrame do
    begin
      AndMask:=((1 shl BitCount)-1) shl Cardinal(BitId);
      BitValue:=(Cardinal(MXCSR) and AndMask) shr Cardinal(BitId);
      Inc(BitValue);
      MXCSR:=TJvMXCSR(   (Cardinal(MXCSR) and (not AndMask))
                      or ((BitValue shl Cardinal(BitId)) and AndMask) );
    end;
  end;
  SetThreadValues;
end;

procedure TSSEForm.GetThreadValues;
  function MakeFlag(Value:Boolean):TObject; overload;
  begin
       if (Value)
         then Result:=TObject(1)
         else Result:=nil;
  end;
  function MakeFlag(Value1:TJvXMMRegister; Value2:TJvXMMRegister):TObject; overload;
  begin
       Result:=MakeFlag(   (Value1.QWords[0]<>Value2.QWords[0])
                        or (Value1.QWords[1]<>Value2.QWords[1]));
  end;
var
  NewVectorFrame:TJvVectorFrame;
  AndMask:Cardinal;
  NewBitValue:Cardinal;
  OldBitValue:Cardinal;
  Index:Integer;
  BitId:TJvMXCSRBits;
begin
  GetVectorContext(FServices.CurrentProcess.CurrentThread.Handle,NewVectorFrame);
  for Index:=0 to ListBoxMXCSR.Items.Count-1 do
    with ListBoxMXCSR, Items do
  begin
    BitId:=TJvMXCSRBits(Cardinal(Objects[Index]) and $FF);
    with MXCSRBitsDescription[BitId] do
    begin
      AndMask:=((1 shl BitCount)-1) shl Cardinal(BitId);
      NewBitValue:=(Cardinal(NewVectorFrame.MXCSR) and AndMask) shr Cardinal(BitId);
      OldBitValue:=(Cardinal(FVectorFrame.MXCSR) and AndMask) shr Cardinal(BitId);
      if (NewBitValue<>OldBitValue) then
        Objects[Index]:=TObject(Cardinal(Objects[Index]) or $100)
      else
        Objects[Index]:=TObject(Cardinal(Objects[Index]) and (not $100));
      Strings[Index]:=IntToStr(NewBitValue);
    end;
  end;
  ListBoxMXCSR.Invalidate;
  ListBoxRegs.Items.Objects[0]:=MakeFlag(NewVectorFrame.MXCSR<>FVectorFrame.MXCSR);
  ListBoxRegs.Items.Strings[0]:=IntToHex(Cardinal(NewVectorFrame.MXCSR),8);
  if (ProcessorVendorID=viAMD) and (asLong in AMDSIMD) then
    Index:=16
  else
    Index:=8;
  while (Index>0) do
  begin
    ListBoxRegs.Items.Objects[Index]:=MakeFlag(NewVectorFrame.XMMRegisters.LongXMM[Index-1],
                                               FVectorFrame.XMMRegisters.LongXMM[Index-1]);
    Dec(Index);
  end;
  FVectorFrame:=NewVectorFrame;
  FormatRegs;
end;

procedure TSSEForm.SetThreadValues;
begin
  SetVectorContext(FServices.CurrentProcess.CurrentThread.Handle,FVectorFrame);
  GetThreadValues;
end;

function TSSEForm.FormatBinary(Value:Int64; nbDigit:Byte):string;
var
  I:Byte;
begin
  Result:=StringOfChar('0',nbDigit);
  for I:=1 to nbDigit do
  begin
    if (Value and 1)<>0
      then Result[nbDigit-I+1]:='1';
    Value:=Value shr 1;
  end;
end;

function TSSEForm.FormatSigned(Value:Int64; nbDigit:Byte):string;
begin
  case Display of
    sdBytes : Result:=IntToStr(ShortInt(Value));
    sdWords : Result:=IntToStr(SmallInt(Value));
    sdDWords : Result:=IntToStr(Integer(Value));
    sdQWords : Result:=IntToStr(Value);
  end;
  Result:=StringOfChar(' ',nbDigit-Length(Result))+Result;
end;

function TSSEForm.FormatUnsigned(Value:Int64; nbDigit:Byte):string;
begin
  case Display of
    sdBytes : Result:=IntToStr(Byte(Value));
    sdWords : Result:=IntToStr(Word(Value));
    sdDWords : Result:=IntToStr(Cardinal(Value));
    sdQWords : Result:=IntToStr(Value);
  end;
  Result:=StringOfChar(' ',nbDigit-Length(Result))+Result;
end;

function TSSEForm.FormatHexa(Value:Int64; nbDigit:Byte):string;
begin
  Result:=IntToHex(Value,nbDigit);
end;

procedure TSSEForm.FormatRegs;
const
  // number of chars per column
  ColumnWidth : array[sdBytes..sdQWords,TSIMDFormat] of Byte =
     // Bytes
       ( ( 8, 4, 3, 2 ),          // (binary, signed, unsigned, hexa)
     // Words
         ( 16, 6, 5, 4 ),
     // DWords
         ( 32, 11, 10, 8 ),
     // QWords
         ( 64, 20, 20, 16 ) );
type
  TFormatFunction = function (Value:int64; nbDigit:Byte):string of Object;
var
  FormatFunc:TFormatFunction;

  function FormatFloat(Value:Extended;MaxLength:Byte):string;
  begin
    Result:=FloatToStr(Value);
    Result:=StringOfChar(' ',MaxLength-Length(Result))+Result;
  end;
  function FormatReg(AReg:TJvXMMRegister):string;
  var
    I:Integer;
  begin
    Result:='';
    case Display of
      sdBytes   : for I:=High(AReg.Bytes) downto Low(AReg.Bytes) do
                    Result:=Result+' '+FormatFunc(AReg.Bytes[I],
                            ColumnWidth[sdBytes,Format]);
      sdWords   : for I:=High(AReg.Words) downto Low(AReg.Words) do
                    Result:=Result+' '+FormatFunc(AReg.Words[I],
                            ColumnWidth[sdWords,Format]);
      sdDWords  : for I:=High(AReg.DWords) downto Low(AReg.DWords) do
                    Result:=Result+' '+FormatFunc(AReg.DWords[I],
                            ColumnWidth[sdDWords,Format]);
      sdQWords  : for I:=High(AReg.QWords) downto Low(AReg.QWords) do
                    Result:=Result+' '+FormatFunc(AReg.QWords[I],
                            ColumnWidth[sdQWords,Format]);
      sdSingles : for I:=High(AReg.Singles) downto Low(AReg.Singles) do
                    Result:=Result+' '+FormatFloat(AReg.Singles[I],21);
      sdDoubles : for I:=High(AReg.Doubles) downto Low(AReg.Doubles) do
                    Result:=Result+' '+FormatFloat(AReg.Doubles[I],22);
    end;
  end;
var
  Index:Integer;
begin
  case Format of
    sfBinary   : FormatFunc:=FormatBinary;
    sfSigned   : FormatFunc:=FormatSigned;
    sfUnsigned : FormatFunc:=FormatUnsigned;
    sfHexa     : FormatFunc:=FormatHexa;
    else Exit;
  end;
  if (ProcessorVendorID=viAMD) and (asLong in AMDSIMD) then
    Index:=16
  else
    Index:=8;
  while (Index>0) do
  begin
    ListBoxRegs.Items.Strings[Index]:=FormatReg(FVectorFrame.XMMRegisters.LongXMM[Index-1]);
    Dec(Index);
  end;
  ListBoxMXCSR.Invalidate;
end;

procedure TSSEForm.MenuItemFormatClick(Sender: TObject);
begin
  Format:=TSIMDFormat((Sender as TMenuItem).Tag);
end;

procedure TSSEForm.SetDisplay(const Value: TSIMDDisplay);
var
  AEnabled:Boolean;
begin
  FDisplay := Value;
  MenuItemBytes.Checked:=Value=sdBytes;
  MenuItemWords.Checked:=Value=sdWords;
  MenuItemDWords.Checked:=Value=sdDWords;
  MenuItemQWords.Checked:=Value=sdQWords;
  MenuItemSingles.Checked:=Value=sdSingles;
  MenuItemDoubles.Checked:=Value=sdDoubles;

  AEnabled:=not (Value in [sdSingles, sdDoubles]);
  MenuItemBinary.Enabled:=AEnabled;
  MenuItemSigned.Enabled:=AEnabled;
  MenuItemUnsigned.Enabled:=AEnabled;
  MenuItemHexa.Enabled:=AEnabled;

  FormatRegs;
end;

procedure TSSEForm.SetFormat(const Value: TSIMDFormat);
begin
  FFormat := Value;
  MenuItemBinary.Checked:=Value=sfBinary;
  MenuItemSigned.Checked:=Value=sfSigned;
  MenuItemUnsigned.Checked:=Value=sfUnsigned;
  MenuItemHexa.Checked:=Value=sfHexa;
  FormatRegs;
end;

procedure TSSEForm.MenuItemDisplayClick(Sender: TObject);
begin
  Display:=TSIMDDisplay((Sender as TMenuItem).Tag);
end;

procedure TSSEForm.DoClose(var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TSSEForm.MenuItemStayOnTopClick(Sender: TObject);
begin
  with (Sender as TMenuItem) do
  begin
    Checked:=not Checked;
    if (Checked)
      then FormStyle:=fsStayOnTop
      else FormStyle:=fsNormal;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
