{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormToHtml;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JvComponent;

type
  TJvFormToHtml = class(TJvComponent)
  private
    FTs: TStringList;
  published
    procedure FormToHtml(Form: TForm; Path: string);
  end;

implementation

{*************************************************}

function FontToCss(Font: TFont): string;
begin
  Result := Format(';font-Size:%d;color:#%d;font-weight:', [Font.Size, Font.Color]);
  if fsBold in Font.Style then
    Result := Result + 'bold;'
  else
    Result := Result + 'normal;';
  Result := Result + 'font-family:' + Font.Name;
end;

{*************************************************}

procedure TJvFormToHtml.FormToHtml(Form: TForm; Path: string);
var
  i, j: Integer;
  c: TComponent;
  st: string;
begin
  FTs := TStringList.Create;
  FTs.Add('<HTML><BODY>');
  for i := 0 to Form.ComponentCount - 1 do
  begin
    c := Form.Components[i];
    st := '';
    if c is TLabel then
    begin
      st := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TLabel).Left, (c as TLabel).Top, (c as TLabel).Height, (c as TLabel).Width]);
      st := st + FontToCss((c as TLabel).Font) + '"';
      st := st + ' TITLE="' + (c as TLabel).Hint + '"';
      st := st + ' NAME=' + (c as TLabel).Name;
      st := st + '>';
      st := st + (c as TLabel).Caption + '</LABEL>';
    end
    else if c is TButton then
    begin
      st := Format('<BUTTON style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TButton).Left, (c as TButton).Top, (c as TButton).Height, (c as TButton).Width]);
      st := st + FontToCss((c as TButton).Font) + '"';
      st := st + ' TITLE="' + (c as TButton).Hint + '"';
      st := st + ' TABORDER=' + IntToStr((c as TButton).TabOrder);
      st := st + ' NAME=' + (c as TButton).Name;
      if (c as TButton).Enabled = False then
        st := st + ' DISABLED';
      st := st + '>';
      st := st + (c as TButton).Caption + '</BUTTON>';
    end
    else if c is TMemo then
    begin
      st := Format('<TEXTAREA style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TMemo).Left, (c as TMemo).Top, (c as TMemo).Height, (c as TMemo).Width]);
      st := st + FontToCss((c as TMemo).Font) + '"';
      st := st + ' TITLE="' + (c as TMemo).Hint + '"';
      if (c as TMemo).ReadOnly then
        st := st + ' ReadOnly';
      if (c as TMemo).Enabled = False then
        st := st + ' DISABLED';
      st := st + ' NAME=' + (c as TMemo).Name;
      st := st + ' TABORDER=' + IntToStr((c as TMemo).TabOrder);
      if (c as TMemo).WordWrap then
        st := st + ' WRAP=PHYSICAL'
      else
        st := st + ' WRAP=OFF';
      st := st + '>';
      st := st + (c as TMemo).Text + '</TEXTAREA>';
    end
    else if c is TCheckBox then
    begin
      st := Format('<INPUT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TCheckBox).Left, (c as TCheckBox).Top, (c as TCheckBox).Height, 10]);
      st := st + FontToCss((c as TCheckBox).Font) + '"';
      st := st + ' TITLE="' + (c as TCheckBox).Hint + '"';
      if (c as TCheckBox).Enabled = False then
        st := st + ' DISABLED';
      if (c as TCheckBox).Checked then
        st := st + ' CHECKED';
      st := st + ' TABORDER=' + IntToStr((c as TCheckBox).TabOrder);
      st := st + ' NAME=' + (c as TCheckBox).Name;
      st := st + ' TYPE="CHECKBOX">';
      FTs.Add(st);
      st := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d', [(c as TCheckBox).Left + 13, (c
          as TCheckBox).Top, (c as TCheckBox).Height, (c as TCheckBox).Width]);
      st := st + FontToCss((c as TCheckBox).Font) + '"';
      st := st + ' TITLE="' + (c as TCheckBox).Hint + '"';
      st := st + '>';
      st := st + (c as TCheckBox).Caption + '</LABEL>';
    end
    else if c is TRadioButton then
    begin
      st := Format('<INPUT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TRadioButton).Left, (c as TRadioButton).Top, (c as TRadioButton).Height, 10]);
      st := st + FontToCss((c as TRadioButton).Font) + '"';
      st := st + ' TITLE="' + (c as TRadioButton).Hint + '"';
      if (c as TRadioButton).Enabled = False then
        st := st + ' DISABLED';
      if (c as TRadioButton).Checked then
        st := st + ' CHECKED';
      st := st + ' NAME=' + (c as TRadioButton).Parent.Name;
      st := st + ' TABORDER=' + IntToStr((c as TRadioButton).TabOrder);
      st := st + ' TYPE="RADIO">';
      FTs.Add(st);
      st := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TRadioButton).Left + 13, (c as TRadioButton).Top, (c as TRadioButton).Height, (c as
          TRadioButton).Width]);
      st := st + FontToCss((c as TRadioButton).Font) + '"';
      st := st + ' TITLE="' + (c as TRadioButton).Hint + '"';
      st := st + '>';
      st := st + (c as TRadioButton).Caption + '</LABEL>';
    end
    else if c is TEdit then
    begin
      st := Format('<INPUT TYPE="TEXT" style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TEdit).Left, (c as TEdit).Top, (c as TEdit).Height, (c as TEdit).Width]);
      st := st + FontToCss((c as TEdit).Font) + '"';
      st := st + ' TITLE="' + (c as TEdit).Hint + '"';
      st := st + ' TABORDER=' + IntToStr((c as TEdit).TabOrder);
      st := st + ' NAME=' + (c as TEdit).Name;
      if (C as TEdit).ReadOnly then
        st := st + ' ReadOnly';
      if (C as Tedit).MaxLength <> 0 then
        st := st + ' MAXLENGTH=' + IntToStr((C as Tedit).maxlength);
      if (c as TEdit).Enabled = False then
        st := st + ' DISABLED';
      st := st + ' Value=' + (c as TEdit).Text;
      st := st + '>';
    end
    else if c is TCombobox then
    begin
      st := Format('<SELECT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TCombobox).Left, (c as TCombobox).Top, (c as TCombobox).Height, (c as TCombobox).Width]);
      st := st + FontToCss((c as TCombobox).Font) + '"';
      st := st + ' TITLE="' + (c as TCombobox).Hint + '"';
      st := st + ' TABORDER=' + IntToStr((c as TCombobox).TabOrder);
      st := st + ' NAME=' + (c as TCombobox).Name;
      if (c as TCombobox).Enabled = False then
        st := st + ' DISABLED';
      st := st + '>';
      FTs.Add(st);
      for j := 0 to (c as TCombobox).Items.Count - 1 do
      begin
        if (c as TCombobox).ItemIndex = j then
          FTs.Add('<OPTION SELECTED>' + (c as TCombobox).Items[j])
        else
          FTs.Add('<OPTION>' + (c as TCombobox).Items[j]);
      end;
      st := '</SELECT>';
    end
    else if c is TListBox then
    begin
      st := Format('<SELECT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(c as TListBox).Left, (c as TListBox).Top, (c as TListBox).Height, (c as TListBox).Width]);
      st := st + FontToCss((c as TListBox).Font) + '"';
      st := st + ' MULTIPLE TITLE="' + (c as TListBox).Hint + '"';
      st := st + ' TABORDER=' + IntToStr((c as TListBox).TabOrder);
      st := st + ' NAME=' + (c as TListBox).Name;
      if (c as TListBox).Enabled = False then
        st := st + ' DISABLED';
      st := st + '>';
      FTs.Add(st);
      for j := 0 to (c as TListBox).Items.Count - 1 do
      begin
        if (c as TListBox).ItemIndex = j then
          FTs.Add('<OPTION SELECTED>' + (c as TListBox).Items[j])
        else
          FTs.Add('<OPTION>' + (c as TListBox).Items[j]);
      end;
      st := '</SELECT>';
    end;

    if st <> '' then
      FTs.Add(st);
  end;
  FTs.Add('</BODY></HTML>');
  FTs.SaveToFile(Path);
  FTs.Free;
end;

end.
