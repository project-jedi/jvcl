{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFullColorList.pas, released on 2004-09-26.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList, Buttons, ImgList,
  FullColorFrm, ColorDialog, ColorSpacess, ColorSpaces, ColorDialogs;

type
  TFormFullColorList = class(TForm)
    JvFullColorDialog: TJvFullColorDialog;
    ListBoxColors: TListBox;
    ActionList: TActionList;
    ActionNew: TAction;
    ActionModify: TAction;
    ActionDelete: TAction;
    ButtonNew: TButton;
    ButtonModify: TButton;
    ButtonDelete: TButton;
    Button4: TButton;
    ButtonOK: TButton;
    BitBtnMoveUp: TBitBtn;
    ActionMoveUp: TAction;
    ActionMoveDown: TAction;
    BitBtnMoveDown: TBitBtn;
    ButtonApply: TButton;
    procedure ActionNewUpdate(Sender: TObject);
    procedure ActionModifyUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionMoveUpUpdate(Sender: TObject);
    procedure ActionMoveDownUpdate(Sender: TObject);
  public
  end;

implementation

{$R *.dfm}

procedure TFormFullColorList.ActionNewUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := True;
end;

procedure TFormFullColorList.ActionModifyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxColors.SelCount = 1;
end;

procedure TFormFullColorList.ActionDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBoxColors.SelCount >= 1;
end;

procedure TFormFullColorList.ActionMoveUpUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (ListBoxColors.SelCount = 1) and not ListBoxColors.Selected[0];
end;

procedure TFormFullColorList.ActionMoveDownUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (ListBoxColors.SelCount = 1) and
    not ListBoxColors.Selected[ListBoxColors.Count - 1];
end;

end.

