{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQFormToHtml;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QGraphics, QControls, QForms, QStdCtrls,
  JvQComponent;

type
  TJvFormToHtml = class(TJvComponent)
  public
    procedure FormToHtml(const Form: TCustomForm; const Filename: string);
  end;

implementation

function FontToCss(const Font: TFont): string;
begin
  Result := Format(';font-Size:%d;color:#%d;font-weight:', [Font.Size, Font.Color]);
  if fsBold in Font.Style then
    Result := Result + 'bold;'
  else
    Result := Result + 'normal;';
  Result := Result + 'font-family:' + Font.Name;
end;

procedure TJvFormToHtml.FormToHtml(const Form: TCustomForm; const Filename: string);
var
  I, J: Integer;
  C: TComponent;
  S, S2, St: string;
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<HTML><BODY>');
    for I := 0 to Form.ComponentCount - 1 do
    begin
      C := Form.Components[I];
      St := '';
      if C is TLabel then
      begin
        St := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TLabel(C).Left, TLabel(C).Top, TLabel(C).Height, TLabel(C).Width]) +
          FontToCss((C as TLabel).Font) + '"' +
          ' TITLE="' + (C as TLabel).Hint + '"' +
          ' NAME=' + (C as TLabel).Name +
          '>' +
          TLabel(C).Caption + '</LABEL>';
      end
      else
      if C is TButton then
      begin
        if not TButton(C).Enabled then
          S := ' DISABLED'
        else
          S := '';

        St := Format('<BUTTON style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TButton(C).Left, TButton(C).Top, TButton(C).Height, TButton(C).Width]) +
          FontToCss(TButton(C).Font) + '"' +
          ' TITLE="' + TButton(C).Hint + '"' +
          ' TABORDER=' + IntToStr(TButton(C).TabOrder) +
          ' NAME=' + TButton(C).Name +
          S +
          '>' +
          TButton(C).Caption + '</BUTTON>';
      end
      else
      if C is TMemo then
      begin
        S := '';
        if TMemo(C).ReadOnly then
          S := S + ' ReadOnly';
        if not TMemo(C).Enabled then
          S := S + ' DISABLED';

        S2 := '';
        if TMemo(C).WordWrap then
          S2 := S2 + ' WRAP=PHYSICAL'
        else
          S2 := S2 + ' WRAP=OFF';

        St := Format('<TEXTAREA style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TMemo(C).Left, TMemo(C).Top, TMemo(C).Height, TMemo(C).Width]) +
          FontToCss(TMemo(C).Font) + '"' +
          ' TITLE="' + TMemo(C).Hint + '"' +
          S +
          ' NAME=' + TMemo(C).Name +
          ' TABORDER=' + IntToStr(TMemo(C).TabOrder) +
          S2 +
          '>' +
          TMemo(C).Text + '</TEXTAREA>';
      end
      else
      if C is TCheckBox then
      begin
        S := '';
        if not TCheckBox(C).Enabled then
          S := S + ' DISABLED';
        if TCheckBox(C).Checked then
          S := S + ' CHECKED';

        St := Format('<INPUT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TCheckBox(C).Left, TCheckBox(C).Top, TCheckBox(C).Height, 10]) +
          FontToCss(TCheckBox(C).Font) + '"' +
          ' TITLE="' + TCheckBox(C).Hint + '"' +
          S +
          ' TABORDER=' + IntToStr(TCheckBox(C).TabOrder) +
          ' NAME=' + TCheckBox(C).Name +
          ' TYPE="CHECKBOX">';
        HTML.Add(St);
        St := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TCheckBox(C).Left + 13, TCheckBox(C).Top, TCheckBox(C).Height, TCheckBox(C).Width]) +
          FontToCss(TCheckBox(C).Font) + '"' +
          ' TITLE="' + TCheckBox(C).Hint + '"' +
          '>' +
          TCheckBox(C).Caption + '</LABEL>';
      end
      else
      if C is TRadioButton then
      begin
        S := '';
        if not TRadioButton(C).Enabled then
          S := S + ' DISABLED';
        if TRadioButton(C).Checked then
          S := S + ' CHECKED';

        St := Format('<INPUT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TRadioButton(C).Left, TRadioButton(C).Top, TRadioButton(C).Height, 10]) +
          FontToCss(TRadioButton(C).Font) + '"' +
          ' TITLE="' + TRadioButton(C).Hint + '"' +
          S +
          ' NAME=' + TRadioButton(C).Parent.Name +
          ' TABORDER=' + IntToStr(TRadioButton(C).TabOrder) +
          ' TYPE="RADIO">';
        HTML.Add(St);
        St := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TRadioButton(C).Left + 13, TRadioButton(C).Top,
           TRadioButton(C).Height, TRadioButton(C).Width]) +
          FontToCss(TRadioButton(C).Font) + '"' +
          ' TITLE="' + TRadioButton(C).Hint + '"' +
          '>' +
          TRadioButton(C).Caption + '</LABEL>';
      end
      else
      if C is TEdit then
      begin
        S := '';
        if TEdit(C).ReadOnly then
          S := S + ' ReadOnly';
        if TEdit(C).MaxLength <> 0 then
          S := S + ' MAXLENGTH=' + IntToStr(TEdit(C).MaxLength);
        if not TEdit(C).Enabled then
          S := S + ' DISABLED';

        St := Format('<INPUT TYPE="TEXT" style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TEdit(C).Left, TEdit(C).Top, TEdit(C).Height, TEdit(C).Width]) +
          FontToCss(TEdit(C).Font) + '"' +
          ' TITLE="' + TEdit(C).Hint + '"' +
          ' TABORDER=' + IntToStr(TEdit(C).TabOrder) +
          ' NAME=' + TEdit(C).Name +
          S +
          ' Value=' + TEdit(C).Text +
          '>';
      end
      else
      if C is TComboBox then
      begin
        if not TComboBox(C).Enabled then
          S := ' DISABLED'
        else
          S := '';

        St := Format('<SELECT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TComboBox(C).Left, TComboBox(C).Top, TComboBox(C).Height, TComboBox(C).Width]) +
          FontToCss(TComboBox(C).Font) + '"' +
          ' TITLE="' + TComboBox(C).Hint + '"' +
          ' TABORDER=' + IntToStr(TComboBox(C).TabOrder) +
          ' NAME=' + TComboBox(C).Name +
          S +
          '>';
        HTML.Add(St);
        for J := 0 to TComboBox(C).Items.Count - 1 do
        begin
          if TComboBox(C).ItemIndex = J then
            HTML.Add('<OPTION SELECTED>' + TComboBox(C).Items[J])
          else
            HTML.Add('<OPTION>' + TComboBox(C).Items[J]);
        end;
        St := '</SELECT>';
      end
      else
      if C is TListBox then
      begin
        if not TListBox(C).Enabled then
          S := ' DISABLED'
        else
          S := '';

        St := Format('<SELECT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
          [TListBox(C).Left, TListBox(C).Top, TListBox(C).Height, TListBox(C).Width]) +
          FontToCss(TListBox(C).Font) + '"' +
          ' MULTIPLE TITLE="' + TListBox(C).Hint + '"' +
          ' TABORDER=' + IntToStr(TListBox(C).TabOrder) +
          ' NAME=' + TListBox(C).Name +
          S + 
          '>';
        HTML.Add(St);
        for J := 0 to TListBox(C).Items.Count - 1 do
        begin
          if TListBox(C).ItemIndex = J then
            HTML.Add('<OPTION SELECTED>' + TListBox(C).Items[J])
          else
            HTML.Add('<OPTION>' + TListBox(C).Items[J]);
        end;
        St := '</SELECT>';
      end;

      if St <> '' then
        HTML.Add(St);
    end;
    HTML.Add('</BODY></HTML>');

    HTML.SaveToFile(Filename);
  finally
    HTML.Free;
  end;
end;

end.

