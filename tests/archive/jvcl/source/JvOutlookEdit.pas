{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOutlookEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvOutlookEdit;

{Component Editors and Registration procedure for TLookOut components }

interface

uses
  Windows, Classes, Dialogs, Graphics, SysUtils, Forms,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type
  TLookOutPageEditor = class(TComponentEditor)
  public
    {$IFDEF COMPILER6_UP}
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    {$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); override;
    {$ENDIF}
    procedure AddButton; virtual;
    procedure SetActive; virtual;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    property Component;
    property Designer;
  end;

  TLookOutEditor = class(TComponentEditor)
  public
    {$IFDEF COMPILER6_UP}
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    {$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); override;
    {$ENDIF}
    procedure AddPage; virtual;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    property Component;
    property Designer;
  end;

  TExpressEditor = class(TComponentEditor)
  public
    {$IFDEF COMPILER6_UP}
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    {$ELSE}
    constructor Create(AComponent: TComponent; ADesigner: IFormDesigner); override;
    {$ENDIF}
    procedure AddButton; virtual;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    property Component;
    property Designer;
  end;

  TLookOutImageIndexProperty = class(TIntegerProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    {$IFNDEF DELPHI6_UP}
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean); override;
    {$ENDIF}
  end;

implementation

uses
  JvLookOut;

//=== TLookOutPageEditor =====================================================

{$IFDEF COMPILER6_UP}
constructor TLookOutPageEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;
{$ELSE}
constructor TLookOutPageEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;
{$ENDIF}

procedure TLookOutPageEditor.Edit;
begin
  if Component <> nil then
    inherited Edit;
end;

procedure TLookOutPageEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      SetActive;
    1:
      AddButton;
    2: ;
    3:
      TJvLookOutPage(Component).UpArrow;
    4:
      TJvLookOutPage(Component).DownArrow;
    // 5: ShowMessage('Lookout components'#13'Copyright © 1997 by Peter Thornqvist; all rights reserved');
  end;
end;

procedure TLookOutPageEditor.SetActive;
var
  C: TJvLookOutPage;
  P: TJvLookOut;
begin
  if Component = nil then
    Exit;
  C := TJvLookOutPage(Component);
  if C.Parent is TJvLookOut then
  begin
    P := TJvLookOut(C.Parent);
    P.ActivePage := C;
  end;
end;

procedure TLookOutPageEditor.AddButton;
var
  Btn: TJvLookOutButton;
begin
  if Component = nil then
    Exit;
  Btn := TJvLookOutPage(Component).AddButton;
  Btn.Name := Designer.UniqueName('LookOutButton');
  Btn.Caption := Btn.Name;
//  Designer.CreateComponent(TJvLookOutButton,Component,0,MaxInt,0,0);
end;

function TLookOutPageEditor.GetVerb(Index: Integer): string;
begin
  if Component = nil then
    Exit;
  case Index of
    0:
      Result := 'Activate';
    1:
      Result := 'Add Button';
    2:
      Result := '-';
    3:
      Result := 'Scroll Up';
    4:
      Result := 'Scroll Down';
    // 5: Result := 'About...';
  end;
end;

function TLookOutPageEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

//=== TLookOutEditor =========================================================

{$IFDEF COMPILER6_UP}
constructor TLookOutEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;
{$ELSE}
constructor TLookOutEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;
{$ENDIF}

procedure TLookOutEditor.Edit;
begin
  if Component <> nil then
    inherited Edit;
end;

procedure TLookOutEditor.ExecuteVerb(Index: Integer);
var
  I: Integer;
begin
  if Component = nil then
    Exit;
  case Index of
    0:
      AddPage;
    1: { next }
      begin
        with Component as TJvLookOut do
          for I := 0 to ControlCount - 1 do
            if (Controls[I] = (Component as TJvLookOut).ActivePage) and
              (I + 1 < ControlCount) and
              (Controls[I + 1] <> nil) and
              (Controls[I + 1] is TJvLookOut) then
            begin
              (Component as TJvLookOut).ActivePage := TJvLookOutPage(Controls[I + 1]);
              Break;
            end;
      end;
    2: { previous }
      begin
        with Component as TJvLookOut do
          for I := ControlCount - 1 downto 0 do
            if (Controls[I] = (Component as TJvLookOut).ActivePage) and
              (I > 0) and
              (Controls[I - 1] <> nil) and
              (Controls[I - 1] is TJvLookOut) then
            begin
              (Component as TJvLookOut).ActivePage := TJvLookOutPage(Controls[I - 1]);
              Break;
            end;
      end;
    //3: ShowMessage('Lookout components'#13'Copyright © 1997 by Peter Thornqvist; all rights reserved');
  end;
end;

procedure TLookOutEditor.AddPage;
var
  Page: TJvLookOutPage;
begin
  if Component = nil then
    Exit;
  Page := TJvLookOut(Component).AddPage;
  Page.Name := Designer.UniqueName('LookOutPage');
  Page.Caption := Page.Name;
end;

function TLookOutEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Page';
    1: Result := 'Next Page';
    2: Result := 'Previous Page';
    //3: Result := 'About...';
  end;
end;

function TLookOutEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

//=== TExpressEditor =========================================================

{$IFDEF COMPILER6_UP}
constructor TExpressEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;
{$ELSE}
constructor TExpressEditor.Create(AComponent: TComponent; ADesigner: IFormDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;
{$ENDIF}

procedure TExpressEditor.AddButton;
var
  Exp: TJvExpress;
  Btn: TJvExpressButton;
begin
  if Component = nil then
    Exit;
  Exp := TJvExpress(Component);
  Btn := TJvExpressButton(Exp.AddButton);
  Btn.Name := Designer.UniqueName('ExpressButton');
  Btn.Caption := Btn.Name;

  with Exp.Controls[Exp.ControlCount - 1] do
  begin
    Width := Exp.Width - 4;
    Left := 0;
  end;

end;

procedure TExpressEditor.Edit;
begin
  inherited Edit;
end;

procedure TExpressEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      AddButton;
    //1: ShowMessage('Lookout components'#13'Copyright © 1997 by Peter Thornqvist; all rights reserved');
  end;
end;

function TExpressEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Add Button';
    //1: Result := 'About...';
  end;
end;

function TExpressEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

//=== TLookOutImageIndexProperty =============================================

function TLookOutImageIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiselect];
end;

procedure TLookOutImageIndexProperty.GetValues(Proc: TGetStrProc);
var
  Btn: TJvLookOutButton;
  I: Integer;
begin
  if GetComponent(0) is TJvCustomLookOutButton then
  begin
    Btn := TJvLookOutButton(GetComponent(0));
    if (Btn.ImageSize = isLarge) and Assigned(Btn.LargeImages) then
      for I := 0 to Btn.LargeImages.Count - 1 do
        Proc(IntToStr(I))
    else
    if (Btn.ImageSize = isSmall) and Assigned(Btn.SmallImages) then
      for I := 0 to Btn.SmallImages.Count - 1 do
        Proc(IntToStr(I))
  end;
end;

{$IFNDEF DELPHI6_UP}

procedure TLookOutImageIndexProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Btn: TJvLookOutButton;
  R: TRect;
begin
  inherited ListDrawValue(Value, ACanvas, ARect, ASelected);
  if (GetComponent(0) is TJvCustomLookOutButton) then
  begin
    R := ARect;
    Btn := TJvLookOutButton(GetComponent(0));
    if (Btn.ImageSize = isLarge) and Assigned(Btn.LargeImages) then
    begin
      ACanvas.FillRect(ARect);
      Btn.LargeImages.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(Value));
      OffsetRect(R, Btn.LargeImages.Width + 2, 0);
      DrawText(ACanvas.Handle, PChar(Value), -1, R, 0);
    end
    else
    if Assigned(Btn.SmallImages) then
    begin
      ACanvas.FillRect(ARect);
      Btn.SmallImages.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(Value));
      OffsetRect(R, Btn.SmallImages.Width + 2, 1);
      DrawText(ACanvas.Handle, PChar(Value), -1, R, 0);
    end;
  end;
end;

procedure TLookOutImageIndexProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  Btn: TJvLookOutButton;
begin
  AHeight := ACanvas.TextHeight(Value) + 2;
  if (GetComponent(0) is TJvCustomLookOutButton) then
  begin
    Btn := TJvLookOutButton(GetComponent(0));
    if (Btn.ImageSize = isLarge) and Assigned(Btn.LargeImages) then
      AHeight := Btn.LargeImages.Height + 2
    else
    if (Btn.ImageSize = isSmall) and Assigned(Btn.SmallImages) then
      AHeight := Btn.SmallImages.Height + 2;
  end;
end;

procedure TLookOutImageIndexProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  Btn: TJvLookOutButton;
begin
  AWidth := ACanvas.TextWidth(Value) + 4;
  if (GetComponent(0) is TJvCustomLookOutButton) then
  begin
    Btn := TJvLookOutButton(GetComponent(0));
    if (Btn.ImageSize = isLarge) and Assigned(Btn.LargeImages) then
      AWidth := Btn.LargeImages.Width + AWidth
    else
    if (Btn.ImageSize = isSmall) and Assigned(Btn.SmallImages) then
      AWidth := Btn.SmallImages.Width + AWidth;
  end;
end;

procedure TLookOutImageIndexProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
//  if GetVisualValue <> '' then
//    ListDrawValue(GetVisualValue, ACanvas, ARect, True)
//  else
  inherited PropDrawValue(ACanvas, ARect, ASelected);
end;

{$ENDIF DELPHI6_UP}

end.

