{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: PageBuilder.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit PageBuilder;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Graphics, StdCtrls, ExtCtrls,
  ComCtrls, ActnList,
  JvWizard,
  Core;


function CreatePage(Page: TJvWizardInteriorPage; Inst: IInstallerPage): TWinControl;
  { returns the active control }
function PreparePage(Page: TJvWizardInteriorPage; Inst: IInstallerPage): TPanel;
procedure DestroyPage(Page: TJvWizardInteriorPage);

implementation

uses
  Main, Utils;

function CreateWelcomePage(Page: TJvWizardInteriorPage; Inst: IWelcomePage): TWinControl; forward;
  { returns the active control }
function CreateMultiChoosePage(Page: TJvWizardInteriorPage; Inst: IMultiChoosePage): TWinControl; forward;
  { returns the active control }
function CreateSingleChoosePage(Page: TJvWizardInteriorPage; Inst: ISingleChoosePage): TWinControl; forward;
  { returns the active control }
function CreateSummaryPage(Page: TJvWizardInteriorPage; Inst: ISummaryPage): TWinControl; forward;
  { returns the active control }


type
  TSingleChooseOptionClick = class(TComponent)
  public
    Index: Integer;
    Page: ISingleChoosePage;
    procedure Click(Sender: TObject);
  end;

  TMultiChooseCheckBoxClick = class(TComponent)
  public
    Index: Integer;
    Page: IMultiChoosePage;
    procedure Click(Sender: TObject);
  end;

procedure TSingleChooseOptionClick.Click(Sender: TObject);
begin
  Page.SetSelectedOption(Index);
end;

procedure TMultiChooseCheckBoxClick.Click(Sender: TObject);
begin
  Page.SetCheckBox(Index, (Sender as TCheckBox).Checked);
end;

// -----------------------------------------------------------------------------

function CreateLayoutPanel(Parent: TWinControl; const Name: string;
  const BoundsRect: TRect): TPanel;
begin
  Result := TPanel.Create(Parent);
  Result.Name := Name;
  Result.BevelInner := bvNone;
  Result.BevelOuter := bvNone;
  Result.BoundsRect := BoundsRect;
  Result.Caption := '';
  Result.Parent := Parent;
end;

function PreparePage(Page: TJvWizardInteriorPage; Inst: IInstallerPage): TPanel;
var
  Title, SubTitle: WideString;
begin
 // caption
  Title := '';
  SubTitle := '';
  Inst.Title(Title, SubTitle);
  Page.Title.Text := Title;
  Page.Subtitle.Text := SubTitle;
  Page.Caption := Title;

  Page.Header.Visible := (Title <> '') or (SubTitle <> '');
  Result := nil;
end;

function CreatePage(Page: TJvWizardInteriorPage; Inst: IInstallerPage): TWinControl;
var
  Y: Integer;
  PageClient: TPanel;
  FocusControl: TWinControl;
begin
  Result := nil;
  if Page.FindComponent('piPageClient') <> nil then // do not localize
    Exit;
  if Page.Header.Visible then
    Y := Page.Header.Height
  else
    Y := 0;
  PageClient := CreateLayoutPanel(Page, 'piPageClient', // do not localize
    Rect(0, Y, Page.ClientWidth, Page.ClientHeight));

  if Supports(Inst, IWelcomePage) then
    Result := CreateWelcomePage(Page, Inst as IWelcomePage)
  else
  if Supports(Inst, ISingleChoosePage) then
    Result := CreateSingleChoosePage(Page, Inst as ISingleChoosePage)
  else
  if Supports(Inst, IMultiChoosePage) then
    Result := CreateMultiChoosePage(Page, Inst as IMultiChoosePage)
  else
  if Supports(Inst, ISummaryPage) then
    Result := CreateSummaryPage(Page, Inst as ISummaryPage)
  ;

 // maybe the user wants some extra controls
  if Supports(Inst, IUserDefinedPage) then
  begin
    FocusControl := (Inst as IUserDefinedPage).SetupPage(PageClient);
    if FocusControl <> nil then
      Result := FocusControl;
  end;
end;

procedure DestroyPage(Page: TJvWizardInteriorPage);

  procedure RemoveActionLists(Owner: TComponent);
  var
    i: Integer;
  begin
    for i := Owner.ComponentCount - 1 downto 0 do
      RemoveActionLists(Owner.Components[i]);
    if (Owner is TCustomActionList) then
      Owner.Free;
  end;

var
  Panel: TPanel;
begin
  Panel := TPanel(Page.FindComponent('piPageClient')); // do not localize
  if Panel <> nil then
  begin
    RemoveActionLists(Panel); // Bug in VCL does not remove the action list from the frame's actionlist list.
    Panel.Free;
  end;
end;

function CreateSingleChooseControls(Parent: TWinControl; Inst: ISingleChoosePage;
  var LockedHorzOrientation: THorzOrientation): TWinControl;
var
  Options: TStrings;
  i, AbsH, X, Y, ps: Integer;
  RadioButton: TRadioButton;
  ItemIndex: Integer;
  S: string;
  HorzOrientation: THorzOrientation;
  Width: Integer;
begin
  Result := nil;
  Options := TStringList.Create;
  try
    Width := Parent.ClientWidth - 16;

    // RadioButtons
    HorzOrientation := hoDefault;
    Inst.Options(Options, HorzOrientation);
    case LockedHorzOrientation of
      hoLeft:
        HorzOrientation := hoRight;
      hoRight:
        HorzOrientation := hoLeft;
      hoCenter:
        Exit; // error
    end;

    if HorzOrientation = hoDefault then
      HorzOrientation := hoLeft;
    LockedHorzOrientation := HorzOrientation;

    if HorzOrientation = hoLeft then
      X := 8
    else if HorzOrientation = hoCenter then
      X := (Parent.ClientWidth - Width) div 2
    else
      X := Parent.ClientWidth - 8 - Width;

    if Options.Count > 0 then
    begin
      ItemIndex := Inst.GetSelectedOption;
      // create radion buttons
      AbsH := (Parent.ClientHeight - 8) div Options.Count;
      Y := 8;
      for i := 0 to Options.Count - 1 do
      begin
        S := Options[i];
        if S <> '' then
        begin
          ps := Pos('|', S);
          if ps = 0 then
            ps := Length(S) + 1;
          RadioButton := TRadioButton.Create(Parent);
          RadioButton.Name := 'piOption_' + IntToStr(i); // do not localize
          RadioButton.Left := X;
          RadioButton.Top := Y;
          RadioButton.Width := Width;
          RadioButton.Caption := Copy(S, 1, ps - 1);
          RadioButton.Hint := Copy(S, ps + 1, MaxInt);
          RadioButton.ShowHint := RadioButton.Hint <> '';
          RadioButton.Parent := Parent;

          with TSingleChooseOptionClick.Create(Parent) do
          begin
            Page := Inst;
            Index := i;
            RadioButton.OnClick := Click;
          end;
          if i = ItemIndex then
          begin
            if Result = nil then
              Result := RadioButton;
            RadioButton.Checked := True;
          end;
          Inst.SetupRadioButton(i, RadioButton);
        end;
        Inc(Y, AbsH);
      end;
    end;
  finally
    Options.Free;
  end;
end;

function CreateMultiChooseControls(Parent: TWinControl; Inst: IMultiChoosePage;
  var LockedHorzOrientation: THorzOrientation): TWinControl;
var
  CheckBoxes: TStrings;
  i, AbsH, X, Y, ps: Integer;
  CheckBox: TCheckBox;
  S: string;
  HorzOrientation: THorzOrientation;
  Width: Integer;
begin
  Result := nil;
  CheckBoxes := TStringList.Create;
  try
    Width := Parent.ClientWidth - 16;

    HorzOrientation := hoDefault;
    Inst.CheckBoxes(CheckBoxes, HorzOrientation);
    case LockedHorzOrientation of
      hoLeft:
        HorzOrientation := hoRight;
      hoRight:
        HorzOrientation := hoLeft;
      hoCenter:
        Exit; // error
    end;

    if HorzOrientation = hoDefault then
      HorzOrientation := hoLeft;
    LockedHorzOrientation := HorzOrientation;

    if HorzOrientation = hoLeft then
      X := 8
    else if HorzOrientation = hoCenter then
      X := (Parent.ClientWidth - Width) div 2
    else
      X := Parent.ClientWidth - 8 - Width;

    if CheckBoxes.Count > 0 then
    begin
      // create check boxes
      AbsH := (Parent.ClientHeight - 8) div CheckBoxes.Count;
      Y := 8;
      for i := 0 to CheckBoxes.Count - 1 do
      begin
        S := CheckBoxes[i];
        if S <> '' then
        begin
          ps := Pos('|', S);
          if ps = 0 then
            ps := Length(S) + 1;
          CheckBox := TCheckBox.Create(Parent);
          CheckBox.Name := 'piCheckBox_' + IntToStr(i);
          CheckBox.Left := X;
          CheckBox.Top := Y;
          CheckBox.Width := Width;
          CheckBox.Caption := Copy(S, 1, ps - 1);
          CheckBox.Hint := Copy(S, ps + 1, MaxInt);
          CheckBox.ShowHint := CheckBox.Hint <> '';
          CheckBox.Parent := Parent;

          with TMultiChooseCheckBoxClick.Create(Parent) do
          begin
            Page := Inst;
            Index := i;
            CheckBox.OnClick := Click;
          end;
          if Result = nil then
            Result := CheckBox;
          CheckBox.Checked := Inst.GetCheckBox(i);
          Inst.SetupCheckBox(i, CheckBox);
        end;
        Inc(Y, AbsH);
      end;
    end;
  finally
    CheckBoxes.Free;
  end;
end;

function CreateWelcomePage(Page: TJvWizardInteriorPage; Inst: IWelcomePage): TWinControl;
var
  Text: WideString;

  Memo: TMemo;
  PageClient, ControlsPanel: TPanel;
  Y: Integer;
  HorzOrientation: THorzOrientation;
  MultiChoosePage: IMultiChoosePage;
begin
  PageClient := TPanel(Page.FindComponent('piPageClient')); // do not localize

  Text := Inst.Text;
  if Text <> '' then
  begin
    Memo := TMemo.Create(PageClient);
    Memo.Parent := PageClient;
    Memo.Name := 'piPageMemo'; // do not localize
    Memo.Left := 8;
    Memo.Top := 8;
    Memo.Width := PageClient.ClientWidth - Memo.Left * 2;
    Memo.Height := (PageClient.ClientHeight - Memo.Top * 2) * 2 div 3;
    Memo.ReadOnly := True;
    Memo.Font.Name := 'Arial';
    Memo.Lines.Text := Text;
    Memo.ScrollBars := ssVertical;
    Memo.Visible := True;
    Y := Memo.Top + Memo.Height;
  end
  else
    Y := 0;
  ControlsPanel := CreateLayoutPanel(PageClient, 'piControlsPanel', // do not localize
    Rect(0, Y, PageClient.ClientWidth, PageClient.ClientHeight));

  HorzOrientation := hoDefault;
  Result := CreateSingleChooseControls(ControlsPanel, Inst, HorzOrientation);
  if Supports(Inst, IMultiChoosePage, MultiChoosePage) then
    CreateMultiChooseControls(ControlsPanel, MultiChoosePage, HorzOrientation);
end;

function CreateSingleChoosePage(Page: TJvWizardInteriorPage; Inst: ISingleChoosePage): TWinControl;
var
  PageClient: TPanel;
  HortOrientation: THorzOrientation;
begin
  PageClient := TPanel(Page.FindComponent('piPageClient')); // do not localize

  HortOrientation := hoDefault;
  Result := CreateSingleChooseControls(PageClient, Inst, HortOrientation);
end;

function CreateMultiChoosePage(Page: TJvWizardInteriorPage; Inst: IMultiChoosePage): TWinControl;
var
  PageClient: TPanel;
  HortOrientation: THorzOrientation;
begin
  PageClient := TPanel(Page.FindComponent('piPageClient')); // do not localize

  HortOrientation := hoDefault;
  Result := CreateMultiChooseControls(PageClient, Inst, HortOrientation);
end;

function CreateSummaryPage(Page: TJvWizardInteriorPage; Inst: ISummaryPage): TWinControl;
var
  PageClient: TPanel;
  ListView: TListView;
  Actions, Comments: TStrings;
  I, MaxWidth, TextWidth, Count: Integer;
  ListItem: TListItem;
begin
  PageClient := TPanel(Page.FindComponent('piPageClient')); // do not localize

  ListView := TListView.Create(PageClient);
  ListView.Parent := PageClient;
  ListView.SetBounds(8, 8, PageClient.ClientWidth - 8 * 2, PageClient.ClientHeight - 8 * 2);
  ListView.ReadOnly := True;
  ListView.ViewStyle := vsReport;
  ListView.RowSelect := True;
  ListView.ShowColumnHeaders := False;

  ListView.SmallImages := FormMain.ImageList;

  MaxWidth := 150;
  Actions := TStringList.Create;
  Comments := TStringList.Create;
  ListView.Items.BeginUpdate;
  try
    Inst.GetSummary(Actions, Comments);
    Count := Actions.Count;
    if Count < Comments.Count then
      Count := Comments.Count;

    for I := 0 to Count - 1 do
    begin
      ListItem := ListView.Items.Add;
      if I >= Actions.Count then
        ListItem.Caption := ''
      else
        ListItem.Caption := Actions[I];

      if ListItem.Caption = '' then
        ListItem.ImageIndex := -1
      else
        ListItem.ImageIndex := 0;


      if I >= Comments.Count then
        ListItem.SubItems.Add('')
      else
      begin
        ListItem.SubItems.Add(Comments[I]);
        TextWidth := ListView.Canvas.TextWidth(Comments[I]);
        if MaxWidth < TextWidth then
          MaxWidth := TextWidth;
      end;
    end;

    ListView.Columns.Add.Width := 150;
    if ListView.ClientWidth < MaxWidth + 50 then
      ListView.Columns.Add.Width := MaxWidth + 50
    else
      ListView.Columns.Add.AutoSize := True;
    ListView.Width := ListView.Width + 1; // force AutoSize
  finally
    ListView.Items.EndUpdate;
    Actions.Free;
    Comments.Free;
  end;

  Result := ListView;
end;


end.
