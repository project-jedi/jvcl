{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Forms.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_Forms;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF VCL}
  SysUtils, Graphics, Controls, Menus, Forms;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  SysUtils, Variants, Qt, QGraphics, QControls, QMenus, QImgList, QForms,
  JvInterpreter_Types;
  {$ENDIF VisualCLX}

{ TControlScrollBar }

{ procedure Assign(Source: TPersistent); }

procedure TControlScrollBar_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Kind: TScrollBarKind }

procedure TControlScrollBar_Read_Kind(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Kind;
end;

{ property Read ScrollPos: Integer }

procedure TControlScrollBar_Read_ScrollPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).ScrollPos;
end;

{ property Read Margin: Word }

procedure TControlScrollBar_Read_Margin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Margin;
end;

{ property Write Margin(Value: Word) }

procedure TControlScrollBar_Write_Margin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Margin := Value;
end;

{ property Read Increment: TScrollBarInc }

procedure TControlScrollBar_Read_Increment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Increment;
end;

{ property Write Increment(Value: TScrollBarInc) }

procedure TControlScrollBar_Write_Increment(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Increment := Value;
end;

{ property Read Range: Integer }

procedure TControlScrollBar_Read_Range(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Range;
end;

{ property Write Range(Value: Integer) }

procedure TControlScrollBar_Write_Range(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Range := Value;
end;

{ property Read Position: Integer }

procedure TControlScrollBar_Read_Position(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Position;
end;

{ property Write Position(Value: Integer) }

procedure TControlScrollBar_Write_Position(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Position := Value;
end;

{ property Read Tracking: Boolean }

procedure TControlScrollBar_Read_Tracking(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Tracking;
end;

{ property Write Tracking(Value: Boolean) }

procedure TControlScrollBar_Write_Tracking(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Tracking := Value;
end;

{ property Read Visible: Boolean }

procedure TControlScrollBar_Read_Visible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TControlScrollBar(Args.Obj).Visible;
end;

{ property Write Visible(Value: Boolean) }

procedure TControlScrollBar_Write_Visible(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TControlScrollBar(Args.Obj).Visible := Value;
end;

{ TScrollingWinControl }

{ constructor Create(AOwner: TComponent) }

procedure TScrollingWinControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScrollingWinControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure DisableAutoRange; }

procedure TScrollingWinControl_DisableAutoRange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollingWinControl(Args.Obj).DisableAutoRange;
end;

{ procedure EnableAutoRange; }

procedure TScrollingWinControl_EnableAutoRange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollingWinControl(Args.Obj).EnableAutoRange;
end;

{ procedure ScrollInView(AControl: TControl); }

procedure TScrollingWinControl_ScrollInView(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollingWinControl(Args.Obj).ScrollInView(V2O(Args.Values[0]) as TControl);
end;

{ property Read HorzScrollBar: TControlScrollBar }

procedure TScrollingWinControl_Read_HorzScrollBar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScrollingWinControl(Args.Obj).HorzScrollBar);
end;

{ property Write HorzScrollBar(Value: TControlScrollBar) }

procedure TScrollingWinControl_Write_HorzScrollBar(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollingWinControl(Args.Obj).HorzScrollBar := V2O(Value) as TControlScrollBar;
end;

{ property Read VertScrollBar: TControlScrollBar }

procedure TScrollingWinControl_Read_VertScrollBar(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScrollingWinControl(Args.Obj).VertScrollBar);
end;

{ property Write VertScrollBar(Value: TControlScrollBar) }

procedure TScrollingWinControl_Write_VertScrollBar(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollingWinControl(Args.Obj).VertScrollBar := V2O(Value) as TControlScrollBar;
end;

{ TScrollBox }

{ constructor Create(AOwner: TComponent) }

procedure TScrollBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScrollBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read BorderStyle: TBorderStyle }

procedure TScrollBox_Read_BorderStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScrollBox(Args.Obj).BorderStyle;
end;

{ property Write BorderStyle(Value: TBorderStyle) }

procedure TScrollBox_Write_BorderStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollBox(Args.Obj).BorderStyle := Value;
end;

{ function CloseQuery: Boolean; }

procedure TCustomForm_CloseQuery(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).CloseQuery;
end;

{ procedure Close; }

procedure TCustomForm_Close(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).Close;
end;

{ procedure DefocusControl(Control: TWinControl; Removing: Boolean); }

procedure TCustomForm_DefocusControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).DefocusControl(V2O(Args.Values[0]) as TWinControl, Args.Values[1]);
end;

{ procedure FocusControl(Control: TWinControl); }

procedure TCustomForm_FocusControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).FocusControl(V2O(Args.Values[0]) as TWinControl);
end;

{ function GetFormImage: TBitmap; }

{$IFDEF VCL}
procedure TCustomForm_GetFormImage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).GetFormImage);
end;
{$ENDIF VCL}

{ procedure Hide; }

procedure TCustomForm_Hide(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).Hide;
end;

{ procedure Print; }

{$IFDEF VCL}
procedure TCustomForm_Print(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).Print;
end;
{$ENDIF VCL}

{ procedure Release; }

procedure TCustomForm_Release(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).Release;
end;

{ procedure SendCancelMode(Sender: TControl); }

{$IFDEF VCL}
procedure TCustomForm_SendCancelMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).SendCancelMode(V2O(Args.Values[0]) as TControl);
end;
{$ENDIF VCL}

{ procedure SetFocus; }

procedure TCustomForm_SetFocus(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).SetFocus;
end;

{ function SetFocusedControl(Control: TWinControl): Boolean; }

procedure TCustomForm_SetFocusedControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).SetFocusedControl(V2O(Args.Values[0]) as TWinControl);
end;

{ procedure Show; }

procedure TCustomForm_Show(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).Show;
end;

{ function ShowModal: Integer; }

procedure TCustomForm_ShowModal(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).ShowModal;
end;

{ property Read Active: Boolean }

procedure TCustomForm_Read_Active(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).Active;
end;

{ property Read ActiveControl: TWinControl }

procedure TCustomForm_Read_ActiveControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).ActiveControl);
end;

{ property Write ActiveControl(Value: TWinControl) }

procedure TCustomForm_Write_ActiveControl(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).ActiveControl := V2O(Value) as TWinControl;
end;

{$IFDEF VCL}

{ property Read ActiveOleControl: TWinControl }

procedure TCustomForm_Read_ActiveOleControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).ActiveOleControl);
end;

{ property Write ActiveOleControl(Value: TWinControl) }

procedure TCustomForm_Write_ActiveOleControl(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).ActiveOleControl := V2O(Value) as TWinControl;
end;

{$ENDIF VCL}

{ property Read Canvas: TCanvas }

procedure TCustomForm_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).Canvas);
end;

{ property Read DropTarget: Boolean }

procedure TCustomForm_Read_DropTarget(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).DropTarget;
end;

{ property Write DropTarget(Value: Boolean) }

procedure TCustomForm_Write_DropTarget(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).DropTarget := Value;
end;

{ property Read HelpFile: string }

procedure TCustomForm_Read_HelpFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).HelpFile;
end;

{ property Write HelpFile(Value: string) }

procedure TCustomForm_Write_HelpFile(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).HelpFile := Value;
end;

{ property Read KeyPreview: Boolean }

procedure TCustomForm_Read_KeyPreview(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).KeyPreview;
end;

{ property Write KeyPreview(Value: Boolean) }

procedure TCustomForm_Write_KeyPreview(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).KeyPreview := Value;
end;

{ property Read Menu: TMainMenu }

procedure TCustomForm_Read_Menu(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomForm(Args.Obj).Menu);
end;

{ property Write Menu(Value: TMainMenu) }

procedure TCustomForm_Write_Menu(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).Menu := V2O(Value) as TMainMenu;
end;

{ property Read ModalResult: TModalResult }

procedure TCustomForm_Read_ModalResult(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).ModalResult;
end;

{ property Write ModalResult(Value: TModalResult) }

procedure TCustomForm_Write_ModalResult(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).ModalResult := Value;
end;

{ property Read WindowState: TWindowState }

procedure TCustomForm_Read_WindowState(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomForm(Args.Obj).WindowState;
end;

{ property Write WindowState(Value: TWindowState) }

procedure TCustomForm_Write_WindowState(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomForm(Args.Obj).WindowState := Value;
end;

{ TForm }

{ constructor Create(AOwner: TComponent) }

procedure TForm_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TForm.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateNew(AOwner: TComponent) }

procedure TForm_CreateNew(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF DELPHI}
  Value := O2V(TForm.CreateNew(V2O(Args.Values[0]) as TComponent));
  {$ELSE}
  Value := O2V(TForm.CreateNew(V2O(Args.Values[0]) as TComponent, 1));
  {$ENDIF DELPHI}
end;

{ procedure ArrangeIcons; }

{$IFDEF VCL}
procedure TForm_ArrangeIcons(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TForm(Args.Obj).ArrangeIcons;
end;
{$ENDIF VCL}

{ procedure Cascade; }

procedure TForm_Cascade(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TForm(Args.Obj).Cascade;
end;

{ procedure Next; }

procedure TForm_Next(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TForm(Args.Obj).Next;
end;

{ procedure Previous; }

procedure TForm_Previous(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TForm(Args.Obj).Previous;
end;

{ procedure Tile; }

procedure TForm_Tile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TForm(Args.Obj).Tile;
end;

{ TDataModule }

{ constructor Create(AOwner: TComponent) }

procedure TDataModule_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDataModule.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateNew(AOwner: TComponent) }

procedure TDataModule_CreateNew(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF DELPHI}
  Value := O2V(TDataModule.CreateNew(V2O(Args.Values[0]) as TComponent));
  {$ELSE}
  Value := O2V(TDataModule.CreateNew(V2O(Args.Values[0]) as TComponent, 1));
  {$ENDIF DELPHI}
end;

{ TScreen }

{ constructor Create(AOwner: TComponent) }

procedure TScreen_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ActiveControl: TWinControl }

procedure TScreen_Read_ActiveControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).ActiveControl);
end;

{ property Read ActiveCustomForm: TCustomForm }

procedure TScreen_Read_ActiveCustomForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).ActiveCustomForm);
end;

{ property Read ActiveForm: TForm }

procedure TScreen_Read_ActiveForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).ActiveForm);
end;

{ property Read CustomFormCount: Integer }

procedure TScreen_Read_CustomFormCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).CustomFormCount;
end;

{ property Read CustomForms[Integer]: TCustomForm }

procedure TScreen_Read_CustomForms(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).CustomForms[Args.Values[0]]);
end;

{ property Read Cursor: TCursor }

procedure TScreen_Read_Cursor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).Cursor;
end;

{ property Write Cursor(Value: TCursor) }

procedure TScreen_Write_Cursor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TScreen(Args.Obj).Cursor := Value;
end;

{ property Read Cursors[Integer]: HCURSOR }

procedure TScreen_Read_Cursors(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TScreen(Args.Obj).Cursors[Args.Values[0]]);
end;

{ property Write Cursors[Integer]: HCURSOR }

procedure TScreen_Write_Cursors(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  TScreen(Args.Obj).Cursors[Args.Values[0]] := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  TScreen(Args.Obj).Cursors[Args.Values[0]] := V2P(Value);
  {$ENDIF VisualCLX}
end;

{ property Read DataModules[Integer]: TDataModule }

procedure TScreen_Read_DataModules(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).DataModules[Args.Values[0]]);
end;

{ property Read DataModuleCount: Integer }

procedure TScreen_Read_DataModuleCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).DataModuleCount;
end;

{$IFDEF VCL}
{ property Read IconFont: TFont }

procedure TScreen_Read_IconFont(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).IconFont);
end;

{ property Write IconFont(Value: TFont) }

procedure TScreen_Write_IconFont(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TScreen(Args.Obj).IconFont := V2O(Value) as TFont;
end;

{$ENDIF VCL}

{ property Read Fonts: TStrings }

procedure TScreen_Read_Fonts(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).Fonts);
end;

{ property Read FormCount: Integer }

procedure TScreen_Read_FormCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).FormCount;
end;

{ property Read Forms[Integer]: TForm }

procedure TScreen_Read_Forms(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).Forms[Args.Values[0]]);
end;

{$IFDEF VCL}

{ property Read Imes: TStrings }

procedure TScreen_Read_Imes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScreen(Args.Obj).Imes);
end;

{ property Read DefaultIme: string }

procedure TScreen_Read_DefaultIme(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).DefaultIme;
end;

{ property Read DefaultKbLayout: HKL }

procedure TScreen_Read_DefaultKbLayout(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TScreen(Args.Obj).DefaultKbLayout);
end;

{$ENDIF VCL}

{ property Read Height: Integer }

procedure TScreen_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).Height;
end;

{ property Read PixelsPerInch: Integer }

procedure TScreen_Read_PixelsPerInch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).PixelsPerInch;
end;

{ property Read Width: Integer }

procedure TScreen_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TScreen(Args.Obj).Width;
end;

{ TApplication }

{ constructor Create(AOwner: TComponent) }

procedure TApplication_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TApplication.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure BringToFront; }

procedure TApplication_BringToFront(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).BringToFront;
end;

{ procedure ControlDestroyed(Control: TControl); }

procedure TApplication_ControlDestroyed(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).ControlDestroyed(V2O(Args.Values[0]) as TControl);
end;

{ procedure CancelHint; }

procedure TApplication_CancelHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).CancelHint;
end;

{ procedure CreateForm(InstanceClass: TComponentClass; var Reference); }

procedure TApplication_CreateForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).CreateForm(TComponentClass(V2C(Args.Values[0])), Args.Values[1]);
end;

{ procedure HandleException(Sender: TObject); }

procedure TApplication_HandleException(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HandleException(V2O(Args.Values[0]));
end;

{ procedure HandleMessage; }

procedure TApplication_HandleMessage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HandleMessage;
end;

{$IFDEF VCL}

{ function HelpCommand(Command: Integer; Data: Longint): Boolean; }

procedure TApplication_HelpCommand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HelpCommand(Args.Values[0], Args.Values[1]);
end;

{ function HelpContext(Context: THelpContext): Boolean; }

procedure TApplication_HelpContext(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HelpContext(Args.Values[0]);
end;

{ function HelpJump(const JumpID: string): Boolean; }

procedure TApplication_HelpJump(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HelpJump(Args.Values[0]);
end;

{$ENDIF VCL}

{ procedure HideHint; }

procedure TApplication_HideHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HideHint;
end;

{ procedure Initialize; }

procedure TApplication_Initialize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Initialize;
end;

{ function MessageBox(Text, Caption: PChar; Flags: Longint): Integer; }

procedure TApplication_MessageBox(var Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  Value := TApplication(Args.Obj).MessageBox(PChar(string(Args.Values[0])), PChar(string(Args.Values[1])),
    Args.Values[2]);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Value := TApplication(Args.Obj).MessageBox(VarToStr(Args.Values[0]), VarToStr(Args.Values[1]),
    TMessageButtons(Byte(V2S(Args.Values[2]))));
  {$ENDIF VisualCLX}
end;

{ procedure Minimize; }

procedure TApplication_Minimize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Minimize;
end;

{ procedure NormalizeAllTopMosts; }

{$IFDEF VCL}
procedure TApplication_NormalizeAllTopMosts(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).NormalizeAllTopMosts;
end;
{$ENDIF VCL}

{ procedure NormalizeTopMosts; }

procedure TApplication_NormalizeTopMosts(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).NormalizeTopMosts;
end;

{ procedure ProcessMessages; }

procedure TApplication_ProcessMessages(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).ProcessMessages;
end;

{ procedure Restore; }

procedure TApplication_Restore(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Restore;
end;

{ procedure RestoreTopMosts; }

procedure TApplication_RestoreTopMosts(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).RestoreTopMosts;
end;

{ procedure Run; }

procedure TApplication_Run(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Run;
end;

{ procedure ShowException(E: Exception); }

procedure TApplication_ShowException(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).ShowException(V2O(Args.Values[0]) as Exception);
end;

{ procedure Terminate; }

procedure TApplication_Terminate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Terminate;
end;

{ property Read Active: Boolean }

procedure TApplication_Read_Active(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).Active;
end;

{ property Read CurrentHelpFile: string }

procedure TApplication_Read_CurrentHelpFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).CurrentHelpFile;
end;

{$IFDEF VCL}

{ property Read DialogHandle: HWnd }

procedure TApplication_Read_DialogHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TApplication(Args.Obj).DialogHandle);
end;

{ property Write DialogHandle(Value: HWnd) }

procedure TApplication_Write_DialogHandle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).DialogHandle := Value;
end;

{$ENDIF VCL}

{ property Read ExeName: string }

procedure TApplication_Read_ExeName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).ExeName;
end;

{ property Read Handle: HWnd }

procedure TApplication_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TApplication(Args.Obj).Handle);
end;

{ property Write Handle(Value: HWnd) }

procedure TApplication_Write_Handle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  {$IFDEF VCL}
  TApplication(Args.Obj).Handle := Value;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  TApplication(Args.Obj).Handle := V2P(Value);
  {$ENDIF VisualCLX}
end;

{ property Read HelpFile: string }

procedure TApplication_Read_HelpFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HelpFile;
end;

{ property Write HelpFile(Value: string) }

procedure TApplication_Write_HelpFile(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HelpFile := Value;
end;

{ property Read Hint: string }

procedure TApplication_Read_Hint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).Hint;
end;

{ property Write Hint(Value: string) }

procedure TApplication_Write_Hint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Hint := Value;
end;

{ property Read HintColor: TColor }

procedure TApplication_Read_HintColor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HintColor;
end;

{ property Write HintColor(Value: TColor) }

procedure TApplication_Write_HintColor(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HintColor := Value;
end;

{ property Read HintPause: Integer }

procedure TApplication_Read_HintPause(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HintPause;
end;

{ property Write HintPause(Value: Integer) }

procedure TApplication_Write_HintPause(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HintPause := Value;
end;

{ property Read HintShortPause: Integer }

procedure TApplication_Read_HintShortPause(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HintShortPause;
end;

{ property Write HintShortPause(Value: Integer) }

procedure TApplication_Write_HintShortPause(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HintShortPause := Value;
end;

{ property Read HintHidePause: Integer }

procedure TApplication_Read_HintHidePause(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).HintHidePause;
end;

{ property Write HintHidePause(Value: Integer) }

procedure TApplication_Write_HintHidePause(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).HintHidePause := Value;
end;

{ property Read Icon: TIcon }

procedure TApplication_Read_Icon(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TApplication(Args.Obj).Icon);
end;

{ property Write Icon(Value: TIcon) }

procedure TApplication_Write_Icon(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Icon := V2O(Value) as TIcon;
end;

{ property Read MainForm: TForm }

procedure TApplication_Read_MainForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TApplication(Args.Obj).MainForm);
end;

{ property Read ShowHint: Boolean }

procedure TApplication_Read_ShowHint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).ShowHint;
end;

{ property Write ShowHint(Value: Boolean) }

procedure TApplication_Write_ShowHint(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).ShowHint := Value;
end;

{ property Read ShowMainForm: Boolean }

procedure TApplication_Read_ShowMainForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).ShowMainForm;
end;

{ property Write ShowMainForm(Value: Boolean) }

procedure TApplication_Write_ShowMainForm(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).ShowMainForm := Value;
end;

{ property Read Terminated: Boolean }

procedure TApplication_Read_Terminated(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).Terminated;
end;

{ property Read Title: string }

procedure TApplication_Read_Title(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).Title;
end;

{ property Write Title(Value: string) }

procedure TApplication_Write_Title(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).Title := Value;
end;

{$IFDEF VCL}

{ property Read UpdateFormatSettings: Boolean }

procedure TApplication_Read_UpdateFormatSettings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).UpdateFormatSettings;
end;

{ property Write UpdateFormatSettings(Value: Boolean) }

procedure TApplication_Write_UpdateFormatSettings(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).UpdateFormatSettings := Value;
end;

{ property Read UpdateMetricSettings: Boolean }

procedure TApplication_Read_UpdateMetricSettings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TApplication(Args.Obj).UpdateMetricSettings;
end;

{ property Write UpdateMetricSettings(Value: Boolean) }

procedure TApplication_Write_UpdateMetricSettings(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TApplication(Args.Obj).UpdateMetricSettings := Value;
end;

{$ENDIF VCL}

{ Application global variable }

procedure JvInterpreter_Application(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(Application);
end;

{ Screen global variable }

procedure JvInterpreter_Screen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(Screen);
end;

{ functions }

{ function GetParentForm(Control: TControl): TCustomForm; }

procedure JvInterpreter_GetParentForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(GetParentForm(V2O(Args.Values[0]) as TControl));
end;

{ function ValidParentForm(Control: TControl): TCustomForm; }

procedure JvInterpreter_ValidParentForm(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(ValidParentForm(V2O(Args.Values[0]) as TControl));
end;

{$IFDEF VCL}

{ function DisableTaskWindows(ActiveWindow: HWnd): Pointer; }

procedure JvInterpreter_DisableTaskWindows(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(DisableTaskWindows(Args.Values[0]));
end;

{ procedure EnableTaskWindows(WindowList: Pointer); }

procedure JvInterpreter_EnableTaskWindows(var Value: Variant; Args: TJvInterpreterArgs);
begin
  EnableTaskWindows(V2P(Args.Values[0]));
end;

{$ENDIF VCL}

{ function IsAccel(VK: Word; const Str: string): Boolean; }

procedure JvInterpreter_IsAccel(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := IsAccel(Args.Values[0], Args.Values[1]);
end;

{$IFDEF VCL}

{ function KeysToShiftState(Keys: Word): TShiftState; }

procedure JvInterpreter_KeysToShiftState(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(KeysToShiftState(Args.Values[0])));
end;

{ function KeyDataToShiftState(KeyData: Longint): TShiftState; }

procedure JvInterpreter_KeyDataToShiftState(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(KeyDataToShiftState(Args.Values[0])));
end;

{ function ForegroundTask: Boolean; }

procedure JvInterpreter_ForegroundTask(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ForegroundTask;
end;

{$ENDIF VCL}

type
  TJvInterpreterFormsEvent = class(TJvInterpreterEvent)
  private
    procedure CloseEvent(Sender: TObject; var Action: TCloseAction);
    procedure CloseQueryEvent(Sender: TObject; var CanClose: Boolean);
    procedure ExceptionEvent(Sender: TObject; E: Exception);
    procedure IdleEvent(Sender: TObject; var Done: Boolean);
    procedure ShowHintEvent(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
  end;

procedure TJvInterpreterFormsEvent.CloseEvent(Sender: TObject; var Action: TCloseAction);
begin
  CallFunction(nil, [O2V(Sender), Action]);
  Action := Args.Values[1];
end;

procedure TJvInterpreterFormsEvent.CloseQueryEvent(Sender: TObject; var CanClose: Boolean);
begin
  CallFunction(nil, [O2V(Sender), CanClose]);
  CanClose := Args.Values[1];
end;

procedure TJvInterpreterFormsEvent.ExceptionEvent(Sender: TObject; E: Exception);
begin
  CallFunction(nil, [O2V(Sender), O2V(E)]);
end;

procedure TJvInterpreterFormsEvent.IdleEvent(Sender: TObject; var Done: Boolean);
begin
  CallFunction(nil, [O2V(Sender), Done]);
  Done := Args.Values[1];
end;

procedure TJvInterpreterFormsEvent.ShowHintEvent(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
begin
{ CallFunction(nil, [HintStr, CanShow, HintInfo]);
  HintStr := Args.Values[0];
  CanShow := Args.Values[1];
  HintInfo := Args.Values[2]; }
  NotImplemented('TShowHintEvent');
 { need define record THintInfo }
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cForms = 'Forms';
begin
  with JvInterpreterAdapter do
  begin
    { TScrollBarKind }
    AddConst(cForms, 'sbHorizontal', Ord(sbHorizontal));
    AddConst(cForms, 'sbVertical', Ord(sbVertical));
    { TControlScrollBar }
    AddClass(cForms, TControlScrollBar, 'TControlScrollBar');
    AddGet(TControlScrollBar, 'Assign', TControlScrollBar_Assign, 1, [varEmpty], varEmpty);
    AddGet(TControlScrollBar, 'Kind', TControlScrollBar_Read_Kind, 0, [varEmpty], varEmpty);
    AddGet(TControlScrollBar, 'ScrollPos', TControlScrollBar_Read_ScrollPos, 0, [varEmpty], varEmpty);
    AddGet(TControlScrollBar, 'Margin', TControlScrollBar_Read_Margin, 0, [varEmpty], varEmpty);
    AddSet(TControlScrollBar, 'Margin', TControlScrollBar_Write_Margin, 0, [varEmpty]);
    AddGet(TControlScrollBar, 'Increment', TControlScrollBar_Read_Increment, 0, [varEmpty], varEmpty);
    AddSet(TControlScrollBar, 'Increment', TControlScrollBar_Write_Increment, 0, [varEmpty]);
    AddGet(TControlScrollBar, 'Range', TControlScrollBar_Read_Range, 0, [varEmpty], varEmpty);
    AddSet(TControlScrollBar, 'Range', TControlScrollBar_Write_Range, 0, [varEmpty]);
    AddGet(TControlScrollBar, 'Position', TControlScrollBar_Read_Position, 0, [varEmpty], varEmpty);
    AddSet(TControlScrollBar, 'Position', TControlScrollBar_Write_Position, 0, [varEmpty]);
    AddGet(TControlScrollBar, 'Tracking', TControlScrollBar_Read_Tracking, 0, [varEmpty], varEmpty);
    AddSet(TControlScrollBar, 'Tracking', TControlScrollBar_Write_Tracking, 0, [varEmpty]);
    AddGet(TControlScrollBar, 'Visible', TControlScrollBar_Read_Visible, 0, [varEmpty], varEmpty);
    AddSet(TControlScrollBar, 'Visible', TControlScrollBar_Write_Visible, 0, [varEmpty]);
    { TScrollingWinControl }
    AddClass(cForms, TScrollingWinControl, 'TScrollingWinControl');
    AddGet(TScrollingWinControl, 'Create', TScrollingWinControl_Create, 1, [varEmpty], varEmpty);
    AddGet(TScrollingWinControl, 'DisableAutoRange', TScrollingWinControl_DisableAutoRange, 0, [varEmpty], varEmpty);
    AddGet(TScrollingWinControl, 'EnableAutoRange', TScrollingWinControl_EnableAutoRange, 0, [varEmpty], varEmpty);
    AddGet(TScrollingWinControl, 'ScrollInView', TScrollingWinControl_ScrollInView, 1, [varEmpty], varEmpty);
    AddGet(TScrollingWinControl, 'HorzScrollBar', TScrollingWinControl_Read_HorzScrollBar, 0, [varEmpty], varEmpty);
    AddSet(TScrollingWinControl, 'HorzScrollBar', TScrollingWinControl_Write_HorzScrollBar, 0, [varEmpty]);
    AddGet(TScrollingWinControl, 'VertScrollBar', TScrollingWinControl_Read_VertScrollBar, 0, [varEmpty], varEmpty);
    AddSet(TScrollingWinControl, 'VertScrollBar', TScrollingWinControl_Write_VertScrollBar, 0, [varEmpty]);
    {$IFDEF VCL}
    { TFormBorderStyle }
    AddConst(cForms, 'bsNone', Ord(bsNone));
    AddConst(cForms, 'bsSingle', Ord(bsSingle));
    AddConst(cForms, 'bsSizeable', Ord(bsSizeable));
    AddConst(cForms, 'bsDialog', Ord(bsDialog));
    AddConst(cForms, 'bsToolWindow', Ord(bsToolWindow));
    AddConst(cForms, 'bsSizeToolWin', Ord(bsSizeToolWin));
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    { TFormBorderStyle }
    AddConst(cForms, 'fbsNone', Ord(fbsNone));
    AddConst(cForms, 'fbsSingle', Ord(fbsSingle));
    AddConst(cForms, 'fbsSizeable', Ord(fbsSizeable));
    AddConst(cForms, 'fbsDialog', Ord(fbsDialog));
    AddConst(cForms, 'fbsToolWindow', Ord(fbsToolWindow));
    AddConst(cForms, 'fbsSizeToolWin', Ord(fbsSizeToolWin));
    {$ENDIF VisualCLX}
    { TScrollBox }
    AddClass(cForms, TScrollBox, 'TScrollBox');
    AddGet(TScrollBox, 'Create', TScrollBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TScrollBox, 'BorderStyle', TScrollBox_Read_BorderStyle, 0, [varEmpty], varEmpty);
    AddSet(TScrollBox, 'BorderStyle', TScrollBox_Write_BorderStyle, 0, [varEmpty]);
    { TWindowState }
    AddConst(cForms, 'wsNormal', Ord(wsNormal));
    AddConst(cForms, 'wsMinimized', Ord(wsMinimized));
    AddConst(cForms, 'wsMaximized', Ord(wsMaximized));
    { TFormStyle }
    AddConst(cForms, 'fsNormal', Ord(fsNormal));
    AddConst(cForms, 'fsMDIChild', Ord(fsMDIChild));
    AddConst(cForms, 'fsMDIForm', Ord(fsMDIForm));
    AddConst(cForms, 'fsStayOnTop', Ord(fsStayOnTop));
    { TBorderIcon }
    AddConst(cForms, 'biSystemMenu', Ord(biSystemMenu));
    AddConst(cForms, 'biMinimize', Ord(biMinimize));
    AddConst(cForms, 'biMaximize', Ord(biMaximize));
    AddConst(cForms, 'biHelp', Ord(biHelp));
    { TPosition }
    AddConst(cForms, 'poDesigned', Ord(poDesigned));
    AddConst(cForms, 'poDefault', Ord(poDefault));
    AddConst(cForms, 'poDefaultPosOnly', Ord(poDefaultPosOnly));
    AddConst(cForms, 'poDefaultSizeOnly', Ord(poDefaultSizeOnly));
    AddConst(cForms, 'poScreenCenter', Ord(poScreenCenter));
    {$IFDEF VCL}
    { TPrintScale }
    AddConst(cForms, 'poNone', Ord(poNone));
    AddConst(cForms, 'poProportional', Ord(poProportional));
    AddConst(cForms, 'poPrintToFit', Ord(poPrintToFit));
    {$ENDIF VCL}
    { TShowAction }
    AddConst(cForms, 'saIgnore', Ord(saIgnore));
    AddConst(cForms, 'saRestore', Ord(saRestore));
    AddConst(cForms, 'saMinimize', Ord(saMinimize));
    AddConst(cForms, 'saMaximize', Ord(saMaximize));
    {$IFDEF VCL}
    { TTileMode }
    AddConst(cForms, 'tbHorizontal', Ord(tbHorizontal));
    AddConst(cForms, 'tbVertical', Ord(tbVertical));
    {$ENDIF VCL}
    { TCloseAction }
    AddConst(cForms, 'caNone', Ord(caNone));
    AddConst(cForms, 'caHide', Ord(caHide));
    AddConst(cForms, 'caFree', Ord(caFree));
    AddConst(cForms, 'caMinimize', Ord(caMinimize));
    { TFormState }
    AddConst(cForms, 'fsCreating', Ord(fsCreating));
    AddConst(cForms, 'fsVisible', Ord(fsVisible));
    AddConst(cForms, 'fsShowing', Ord(fsShowing));
    AddConst(cForms, 'fsModal', Ord(fsModal));
    {$IFDEF VCL}
    AddConst(cForms, 'fsCreatedMDIChild', Ord(fsCreatedMDIChild));
    {$ENDIF VCL}
    {$IFDEF COMPILER5_UP}
    AddConst(cForms, 'fsActivated', Ord(fsActivated));
    {$ENDIF COMPILER5_UP}
    { TCustomForm }
    AddClass(cForms, TCustomForm, 'TCustomForm');
    AddGet(TCustomForm, 'Close', TCustomForm_Close, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'CloseQuery', TCustomForm_CloseQuery, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'DefocusControl', TCustomForm_DefocusControl, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomForm, 'FocusControl', TCustomForm_FocusControl, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomForm, 'GetFormImage', TCustomForm_GetFormImage, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomForm, 'Hide', TCustomForm_Hide, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomForm, 'Print', TCustomForm_Print, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomForm, 'Release', TCustomForm_Release, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TCustomForm, 'SendCancelMode', TCustomForm_SendCancelMode, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TCustomForm, 'SetFocus', TCustomForm_SetFocus, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'SetFocusedControl', TCustomForm_SetFocusedControl, 1, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'Show', TCustomForm_Show, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'ShowModal', TCustomForm_ShowModal, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'Active', TCustomForm_Read_Active, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'ActiveControl', TCustomForm_Read_ActiveControl, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'ActiveControl', TCustomForm_Write_ActiveControl, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TCustomForm, 'ActiveOleControl', TCustomForm_Read_ActiveOleControl, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'ActiveOleControl', TCustomForm_Write_ActiveOleControl, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TCustomForm, 'Canvas', TCustomForm_Read_Canvas, 0, [varEmpty], varEmpty);
    AddGet(TCustomForm, 'DropTarget', TCustomForm_Read_DropTarget, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'DropTarget', TCustomForm_Write_DropTarget, 0, [varEmpty]);
    AddGet(TCustomForm, 'HelpFile', TCustomForm_Read_HelpFile, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'HelpFile', TCustomForm_Write_HelpFile, 0, [varEmpty]);
    AddGet(TCustomForm, 'KeyPreview', TCustomForm_Read_KeyPreview, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'KeyPreview', TCustomForm_Write_KeyPreview, 0, [varEmpty]);
    AddGet(TCustomForm, 'Menu', TCustomForm_Read_Menu, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'Menu', TCustomForm_Write_Menu, 0, [varEmpty]);
    AddGet(TCustomForm, 'ModalResult', TCustomForm_Read_ModalResult, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'ModalResult', TCustomForm_Write_ModalResult, 0, [varEmpty]);
    AddGet(TCustomForm, 'WindowState', TCustomForm_Read_WindowState, 0, [varEmpty], varEmpty);
    AddSet(TCustomForm, 'WindowState', TCustomForm_Write_WindowState, 0, [varEmpty]);
    { TForm }
    AddClass(cForms, TForm, 'TForm');
    AddGet(TForm, 'Create', TForm_Create, 1, [varEmpty], varEmpty);
    {$IFDEF DELPHI}
    AddGet(TForm, 'CreateNew', TForm_CreateNew, 1, [varEmpty], varEmpty);
    {$ELSE}
    AddGet(TForm, 'CreateNew', TForm_CreateNew, 2, [varEmpty, varEmpty], varEmpty);
    {$ENDIF DELPHI}
    {$IFDEF VCL}
    AddGet(TForm, 'ArrangeIcons', TForm_ArrangeIcons, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TForm, 'Cascade', TForm_Cascade, 0, [varEmpty], varEmpty);
    AddGet(TForm, 'Next', TForm_Next, 0, [varEmpty], varEmpty);
    AddGet(TForm, 'Previous', TForm_Previous, 0, [varEmpty], varEmpty);
    AddGet(TForm, 'Tile', TForm_Tile, 0, [varEmpty], varEmpty);
    { TDataModule }
    AddClass(cForms, TDataModule, 'TDataModule');
    AddGet(TDataModule, 'Create', TDataModule_Create, 1, [varEmpty], varEmpty);
    {$IFDEF DELPHI}
    AddGet(TDataModule, 'CreateNew', TDataModule_CreateNew, 1, [varEmpty], varEmpty);
    {$ELSE}
    AddGet(TDataModule, 'CreateNew', TDataModule_CreateNew, 2, [varEmpty, varEmpty], varEmpty);
    {$ENDIF DELPHI}
    { TScreen }
    AddClass(cForms, TScreen, 'TScreen');
    AddGet(TScreen, 'Create', TScreen_Create, 1, [varEmpty], varEmpty);
    AddGet(TScreen, 'ActiveControl', TScreen_Read_ActiveControl, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'ActiveCustomForm', TScreen_Read_ActiveCustomForm, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'ActiveForm', TScreen_Read_ActiveForm, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'CustomFormCount', TScreen_Read_CustomFormCount, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'CustomForms', TScreen_Read_CustomForms, 1, [varEmpty], varEmpty);
    AddGet(TScreen, 'Cursor', TScreen_Read_Cursor, 0, [varEmpty], varEmpty);
    AddSet(TScreen, 'Cursor', TScreen_Write_Cursor, 0, [varEmpty]);
    AddGet(TScreen, 'Cursors', TScreen_Read_Cursors, 1, [varEmpty], varEmpty);
    AddSet(TScreen, 'Cursors', TScreen_Write_Cursors, 1, [varNull]);
    AddGet(TScreen, 'DataModules', TScreen_Read_DataModules, 1, [varEmpty], varEmpty);
    AddGet(TScreen, 'DataModuleCount', TScreen_Read_DataModuleCount, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TScreen, 'IconFont', TScreen_Read_IconFont, 0, [varEmpty], varEmpty);
    AddSet(TScreen, 'IconFont', TScreen_Write_IconFont, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TScreen, 'Fonts', TScreen_Read_Fonts, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'FormCount', TScreen_Read_FormCount, 0, [varEmpty], varEmpty);
    AddIGet(TScreen, cForms, TScreen_Read_Forms, 1, [varEmpty], varEmpty); // ivan_ra
    {$IFDEF VCL}
    AddGet(TScreen, 'Imes', TScreen_Read_Imes, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'DefaultIme', TScreen_Read_DefaultIme, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'DefaultKbLayout', TScreen_Read_DefaultKbLayout, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TScreen, 'Height', TScreen_Read_Height, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'PixelsPerInch', TScreen_Read_PixelsPerInch, 0, [varEmpty], varEmpty);
    AddGet(TScreen, 'Width', TScreen_Read_Width, 0, [varEmpty], varEmpty);
    { TTimerMode }
    AddConst(cForms, 'tmShow', Ord(tmShow));
    AddConst(cForms, 'tmHide', Ord(tmHide));
    { TApplication }
    AddClass(cForms, TApplication, 'TApplication');
    AddGet(TApplication, 'Create', TApplication_Create, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'BringToFront', TApplication_BringToFront, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'ControlDestroyed', TApplication_ControlDestroyed, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'CancelHint', TApplication_CancelHint, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'CreateForm', TApplication_CreateForm, 2, [varEmpty, varByRef], varEmpty);
    AddGet(TApplication, 'HandleException', TApplication_HandleException, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'HandleMessage', TApplication_HandleMessage, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TApplication, 'HelpCommand', TApplication_HelpCommand, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TApplication, 'HelpContext', TApplication_HelpContext, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'HelpJump', TApplication_HelpJump, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TApplication, 'HideHint', TApplication_HideHint, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'Initialize', TApplication_Initialize, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'MessageBox', TApplication_MessageBox, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TApplication, 'Minimize', TApplication_Minimize, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TApplication, 'NormalizeAllTopMosts', TApplication_NormalizeAllTopMosts, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddGet(TApplication, 'NormalizeTopMosts', TApplication_NormalizeTopMosts, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'ProcessMessages', TApplication_ProcessMessages, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'Restore', TApplication_Restore, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'RestoreTopMosts', TApplication_RestoreTopMosts, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'Run', TApplication_Run, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'ShowException', TApplication_ShowException, 1, [varEmpty], varEmpty);
    AddGet(TApplication, 'Terminate', TApplication_Terminate, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'Active', TApplication_Read_Active, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'CurrentHelpFile', TApplication_Read_CurrentHelpFile, 0, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddGet(TApplication, 'DialogHandle', TApplication_Read_DialogHandle, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'DialogHandle', TApplication_Write_DialogHandle, 0, [varEmpty]);
    {$ENDIF VCL}
    AddGet(TApplication, 'ExeName', TApplication_Read_ExeName, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'Handle', TApplication_Read_Handle, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'Handle', TApplication_Write_Handle, 0, [varEmpty]);
    AddGet(TApplication, 'HelpFile', TApplication_Read_HelpFile, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'HelpFile', TApplication_Write_HelpFile, 0, [varEmpty]);
    AddGet(TApplication, 'Hint', TApplication_Read_Hint, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'Hint', TApplication_Write_Hint, 0, [varEmpty]);
    AddGet(TApplication, 'HintColor', TApplication_Read_HintColor, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'HintColor', TApplication_Write_HintColor, 0, [varEmpty]);
    AddGet(TApplication, 'HintPause', TApplication_Read_HintPause, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'HintPause', TApplication_Write_HintPause, 0, [varEmpty]);
    AddGet(TApplication, 'HintShortPause', TApplication_Read_HintShortPause, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'HintShortPause', TApplication_Write_HintShortPause, 0, [varEmpty]);
    AddGet(TApplication, 'HintHidePause', TApplication_Read_HintHidePause, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'HintHidePause', TApplication_Write_HintHidePause, 0, [varEmpty]);
    AddGet(TApplication, 'Icon', TApplication_Read_Icon, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'Icon', TApplication_Write_Icon, 0, [varEmpty]);
    AddGet(TApplication, 'MainForm', TApplication_Read_MainForm, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'ShowHint', TApplication_Read_ShowHint, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'ShowHint', TApplication_Write_ShowHint, 0, [varEmpty]);
    AddGet(TApplication, 'ShowMainForm', TApplication_Read_ShowMainForm, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'ShowMainForm', TApplication_Write_ShowMainForm, 0, [varEmpty]);
    AddGet(TApplication, 'Terminated', TApplication_Read_Terminated, 0, [varEmpty], varEmpty);
    AddGet(TApplication, 'Title', TApplication_Read_Title, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'Title', TApplication_Write_Title, 0, [varEmpty]);
    {$IFDEF VCL}
    AddGet(TApplication, 'UpdateFormatSettings', TApplication_Read_UpdateFormatSettings, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'UpdateFormatSettings', TApplication_Write_UpdateFormatSettings, 0, [varEmpty]);
    AddGet(TApplication, 'UpdateMetricSettings', TApplication_Read_UpdateMetricSettings, 0, [varEmpty], varEmpty);
    AddSet(TApplication, 'UpdateMetricSettings', TApplication_Write_UpdateMetricSettings, 0, [varEmpty]);
    {$ENDIF VCL}

    AddFunction(cForms, 'Application', JvInterpreter_Application, 0, [varEmpty], varEmpty);
    AddFunction(cForms, 'Screen', JvInterpreter_Screen, 0, [varEmpty], varEmpty);

    AddFunction(cForms, 'GetParentForm', JvInterpreter_GetParentForm, 1, [varEmpty], varEmpty);
    AddFunction(cForms, 'ValidParentForm', JvInterpreter_ValidParentForm, 1, [varEmpty], varEmpty);
    {$IFDEF VCL}
    AddFunction(cForms, 'DisableTaskWindows', JvInterpreter_DisableTaskWindows, 1, [varEmpty], varEmpty);
    AddFunction(cForms, 'EnableTaskWindows', JvInterpreter_EnableTaskWindows, 1, [varEmpty], varEmpty);
    {$ENDIF VCL}
    AddFunction(cForms, 'IsAccel', JvInterpreter_IsAccel, 2, [varEmpty, varEmpty], varEmpty);
    {$IFDEF VCL}
    AddFunction(cForms, 'KeysToShiftState', JvInterpreter_KeysToShiftState, 1, [varEmpty], varEmpty);
    AddFunction(cForms, 'KeyDataToShiftState', JvInterpreter_KeyDataToShiftState, 1, [varEmpty], varEmpty);
    AddFunction(cForms, 'ForegroundTask', JvInterpreter_ForegroundTask, 0, [varEmpty], varEmpty);
    {$ENDIF VCL}

    AddHandler(cForms, 'TCloseEvent', TJvInterpreterFormsEvent, @TJvInterpreterFormsEvent.CloseEvent);
    AddHandler(cForms, 'TCloseQueryEvent', TJvInterpreterFormsEvent, @TJvInterpreterFormsEvent.CloseQueryEvent);
    AddHandler(cForms, 'TExceptionEvent', TJvInterpreterFormsEvent, @TJvInterpreterFormsEvent.ExceptionEvent);
    AddHandler(cForms, 'TIdleEvent', TJvInterpreterFormsEvent, @TJvInterpreterFormsEvent.IdleEvent);
    AddHandler(cForms, 'TShowHintEvent', TJvInterpreterFormsEvent, @TJvInterpreterFormsEvent.ShowHintEvent);
  end;
  RegisterClasses([TScrollBox]);
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

