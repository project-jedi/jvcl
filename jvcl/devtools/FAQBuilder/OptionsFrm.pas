{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Peter Thörnqvist

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

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
unit OptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExExtCtrls, JvComponent, JvRollOut, StdCtrls,
  JvExStdCtrls, JvRichEdit, Mask, JvExMask, JvToolEdit, ComCtrls, JvEdit,
  JvDotNetControls, Buttons;

type
  TfrmOptions = class(TForm)
    pcOptions: TPageControl;
    btnOK: TJvDotNetButton;
    btnCancel: TJvDotNetButton;
    tabGeneral: TTabSheet;
    tabTemplates: TTabSheet;
    Label1: TLabel;
    edQImage: TJvDotNetFilenameEdit;
    Label2: TLabel;
    edAImage: TJvDotNetFilenameEdit;
    Label3: TLabel;
    edStyleSheet: TJvDotNetFilenameEdit;
    Label4: TLabel;
    edImagePath: TJvDotNetEdit;
    Label5: TLabel;
    edStyleSheetPath: TJvDotNetEdit;
    roFooter: TJvRollOut;
    roHeader: TJvRollOut;
    reHeader: TJvDotNetRichEdit;
    reFooter: TJvDotNetRichEdit;
    roItem: TJvRollOut;
    reItem: TJvDotNetRichEdit;
    Label6: TLabel;
    edTitle: TJvDotNetEdit;
    procedure StripPathFromFilename(Sender: TObject; var Name: String;
      var Action: Boolean);
    procedure FormResize(Sender: TObject);
    procedure roHeaderExpand(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function Edit:boolean;
  end;


implementation
uses
  FAQGlobals;

{$R *.dfm}

class function TfrmOptions.Edit: boolean;
var
  frm:TfrmOptions;
begin
  frm := Self.Create(Application);
  try
    frm.edQImage.Text := FAQOptions.QImage;
    frm.edAImage.Text := FAQOptions.AImage;
    frm.edImagePath.Text := FAQOptions.ImagePath;
    frm.edStyleSheet.Text := FAQOptions.Stylesheet;
    frm.edStyleSheetPath.Text := FAQOptions.StylePath;
    frm.edTitle.Text          := FAQOptions.Title;
    frm.reHeader.Lines := FAQOptions.Header;
    frm.reItem.Lines := FAQOptions.Item;
    frm.reFooter.Lines := FAQOptions.Footer;
    frm.pcOptions.ActivePageIndex := 0;
    frm.roHeader.Expand;
    Result := frm.ShowModal = mrOK;
    if Result then
    begin
      FAQOptions.QImage := frm.edQImage.Text;
      FAQOptions.AImage := frm.edAImage.Text;
      FAQOptions.ImagePath := frm.edImagePath.Text;
      FAQOptions.Stylesheet := frm.edStyleSheet.Text;
      FAQOptions.StylePath := frm.edStyleSheetPath.Text;
      FAQOptions.Title := frm.edTitle.Text;
      FAQOptions.Header := frm.reHeader.Lines;
      FAQOptions.Item := frm.reItem.Lines;
      FAQOptions.Footer := frm.reFooter.Lines;
    end;
  finally
    frm.Free;
  end;
end;

procedure TfrmOptions.StripPathFromFilename(Sender: TObject;
  var Name: String; var Action: Boolean);
begin
  Name := ExtractFilename(Name);
end;

procedure TfrmOptions.FormResize(Sender: TObject);
var i:integer;
begin
  with tabTemplates do
    for i := 0 to ControlCount - 1 do
      if (Controls[i] is TJvRollOut) and not TJvRollOut(Controls[i]).Collapsed then
      begin
        TJvRollOut(Controls[i]).Height := ClientHeight - 44;
        Exit;
      end;
end;

procedure TfrmOptions.roHeaderExpand(Sender: TObject);
begin
  with Sender as TJvRollOut do
    Height := tabTemplates.ClientHeight - 44;
end;

end.
