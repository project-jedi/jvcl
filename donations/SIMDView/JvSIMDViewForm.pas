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
  Dialogs, StdCtrls, ToolsApi, Grids, ExtCtrls, Menus, JclSysInfo, JvSIMDUtils;

type
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
    MenuItemModify: TMenuItem;
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
    procedure ListBoxRegsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItemModifyClick(Sender: TObject);
  private
    FServices:IOTADebuggerServices;
    FVectorFrame:TJvVectorFrame;
    FDisplay:TJvSIMDDisplay;
    FFormat:TJvSIMDFormat;
    FCpuInfo: TCpuInfo;
    FCpuInfoValid: Boolean;
    procedure SetDisplay(const Value: TJvSIMDDisplay);
    procedure SetFormat(const Value: TJvSIMDFormat);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create (AOwner:TComponent;
     AServices:IOTADebuggerServices); reintroduce;
    destructor Destroy; override;
    procedure SetThreadValues;
    procedure GetThreadValues;
    procedure FormatRegs;
    function CpuInfo: TCpuInfo;
    property Format:TJvSIMDFormat read FFormat write SetFormat;
    property Display:TJvSIMDDisplay read FDisplay write SetDisplay;
  end;

var
  SSEForm: TSSEForm;

implementation

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  JvSIMDModifyForm;

{$R *.dfm}

constructor TSSEForm.Create(AOwner: TComponent;
  AServices: IOTADebuggerServices);
var
  I:TJvMXCSRBits;
  J:Integer;
begin
  inherited Create(AOwner);
  FCpuInfoValid := False;
  FServices:=AServices;
  ListBoxMXCSR.Items.Clear;
  with CpuInfo do
    for I:=mbInvalidOperationException to mbFlushToZero do
      if (MXCSRBitsDescription[I].BitCount<>0) then
      ListBoxMXCSR.Items.AddObject('0',TObject(I));
  ListBoxRegs.Items.Clear;
  with CpuInfo do
    if (CpuType = CPU_TYPE_AMD) and (HasExtendedInfo)
      and ((AMDSpecific.ExFeatures and EAMD_LONG_FLAG)<>0) then
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

function TSSEForm.CpuInfo: TCpuInfo;
begin
  if not FCpuInfoValid then
  begin
    JclSysInfo.GetCpuInfo(FCpuInfo);
    FCpuInfoValid := True;
  end;
  Result := FCpuInfo;
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
  AIndex: Integer;
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

procedure TSSEForm.ListBoxRegsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbRight) then
    with (Sender as TListBox) do
  begin
    MenuItemModify.Enabled := ItemAtPos(Point(X,Y),True)>0;
    with ClientToScreen(Point(X,Y)) do
      PopupMenu.Popup(X,Y);
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
  with CpuInfo do
    if (CpuType = CPU_TYPE_AMD) and (HasExtendedInfo)
      and ((AMDSpecific.ExFeatures and EAMD_LONG_FLAG)<>0) then
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

procedure TSSEForm.FormatRegs;
  function FormatReg(AReg:TJvXMMRegister):string;
  var
    I:Integer;
    Value: TJvSIMDValue;
  begin
    Result:='';
    Value.Display := Display;
    case Display of
      sdBytes   : for I:=High(AReg.Bytes) downto Low(AReg.Bytes) do
                  begin
                    Value.ValueByte := AReg.Bytes[I];
                    Result:=Result+' '+FormatValue(Value,Format);
                  end;
      sdWords   : for I:=High(AReg.Words) downto Low(AReg.Words) do
                  begin
                    Value.ValueWord := AReg.Words[I];
                    Result:=Result+' '+FormatValue(Value,Format);
                  end;
      sdDWords  : for I:=High(AReg.DWords) downto Low(AReg.DWords) do
                  begin
                    Value.ValueDWord := AReg.DWords[I];
                    Result:=Result+' '+FormatValue(Value,Format);
                  end;
      sdQWords  : for I:=High(AReg.QWords) downto Low(AReg.QWords) do
                  begin
                    Value.ValueQWord := AReg.QWords[I];
                    Result:=Result+' '+FormatValue(Value,Format);
                  end;
      sdSingles : for I:=High(AReg.Singles) downto Low(AReg.Singles) do
                  begin
                    Value.ValueSingle := AReg.Singles[I];
                    Result:=Result+' '+FormatValue(Value,sfBinary);
                  end;
      sdDoubles : for I:=High(AReg.Doubles) downto Low(AReg.Doubles) do
                  begin
                    Value.ValueDouble := AReg.Doubles[I];
                    Result:=Result+' '+FormatValue(Value,sfBinary);
                  end;
    end;
  end;
var
  Index:Integer;
begin
  with CpuInfo do
    if (CpuType = CPU_TYPE_AMD) and (HasExtendedInfo)
      and ((AMDSpecific.ExFeatures and EAMD_LONG_FLAG)<>0) then
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
  Format:=TJvSIMDFormat((Sender as TMenuItem).Tag);
end;

procedure TSSEForm.SetDisplay(const Value: TJvSIMDDisplay);
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

procedure TSSEForm.SetFormat(const Value: TJvSIMDFormat);
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
  Display:=TJvSIMDDisplay((Sender as TMenuItem).Tag);
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

procedure TSSEForm.MenuItemModifyClick(Sender: TObject);
begin
  with TJvSIMDModifyFrm.Create(Self) do
  begin
    Icon.Assign(Self.Icon);
    Execute;
    Free;
  end;
end;

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
