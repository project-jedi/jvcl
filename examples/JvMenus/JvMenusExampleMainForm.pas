{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvMenusExampleMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, JvMenus, ImgList, ExtCtrls, ToolWin, ComCtrls, JvToolBar,
  StdCtrls, JvExComCtrls;

type
  TJvMenusExampleMainFrm = class(TForm)
    jmnMain: TJvMainMenu;
    File1: TMenuItem;
    Try1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Other1: TMenuItem;
    Sub11: TMenuItem;
    Hello1: TMenuItem;
    Plaf1: TMenuItem;
    Yop1: TMenuItem;
    Nice1: TMenuItem;
    Checked1: TMenuItem;
    N11: TMenuItem;
    Radio11: TMenuItem;
    Radio21: TMenuItem;
    Radio31: TMenuItem;
    imlImages: TImageList;
    jpmPopup: TJvPopupMenu;
    pnlPopup: TPanel;
    Popup11: TMenuItem;
    Popup21: TMenuItem;
    PopupSub1: TMenuItem;
    Yop2: TMenuItem;
    Yip1: TMenuItem;
    N2: TMenuItem;
    SUb1: TMenuItem;
    SubAgain1: TMenuItem;
    Checked2: TMenuItem;
    CheckedInSub1: TMenuItem;
    AfterPSub1: TMenuItem;
    jtbMenus: TJvToolBar;
    btnAddItems: TButton;
    btnChangeCaption: TButton;
    pnlMarginPopup: TPanel;
    jpmMarginPopup: TJvPopupMenu;
    Test1: TMenuItem;
    Testagain1: TMenuItem;
    jipMarginPainter: TJvStandardMenuItemPainter;
    memExplanation: TMemo;
    N3: TMenuItem;
    imlSubImages: TImageList;
    procedure Exit1Click(Sender: TObject);
    procedure btnAddItemsClick(Sender: TObject);
    procedure btnChangeCaptionClick(Sender: TObject);
    procedure Try1Click(Sender: TObject);
  end;

var
  JvMenusExampleMainFrm: TJvMenusExampleMainFrm;

implementation

{$R *.dfm}

procedure TJvMenusExampleMainFrm.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TJvMenusExampleMainFrm.btnAddItemsClick(Sender: TObject);
var item : TMenuItem;
begin
  item := TMenuItem.Create(Self);
  item.Caption := 'Add';
  jmnMain.Items.Add(item);
  item := TMenuItem.Create(Self);
  item.Caption := 'Below Add';
  jmnMain.Items[3].Add(item);
end;

procedure TJvMenusExampleMainFrm.btnChangeCaptionClick(Sender: TObject);
begin
  // simple test to check tabs are taken into account correctly
  Try1.Caption := '1'#9'ReFile';
end;

procedure TJvMenusExampleMainFrm.Try1Click(Sender: TObject);
begin
  ShowMessage('Nice try !');
end;

end.