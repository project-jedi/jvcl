{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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

{$I jvcl.inc}

unit JvQFormToHtml;

interface

uses
  SysUtils, Classes,
  
  
  QGraphics, QControls, QForms, QStdCtrls,
  
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
  St: string;
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  HTML.Add('<HTML><BODY>');
  for I := 0 to Form.ComponentCount - 1 do
  begin
    C := Form.Components[I];
    St := '';
    if C is TLabel then
    begin
      St := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TLabel).Left, (C as TLabel).Top, (C as TLabel).Height, (C as TLabel).Width]);
      St := St + FontToCss((C as TLabel).Font) + '"';
      St := St + ' TITLE="' + (C as TLabel).Hint + '"';
      St := St + ' NAME=' + (C as TLabel).Name;
      St := St + '>';
      St := St + (C as TLabel).Caption + '</LABEL>';
    end
    else
    if C is TButton then
    begin
      St := Format('<BUTTON style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TButton).Left, (C as TButton).Top, (C as TButton).Height, (C as TButton).Width]);
      St := St + FontToCss((C as TButton).Font) + '"';
      St := St + ' TITLE="' + (C as TButton).Hint + '"';
      St := St + ' TABORDER=' + IntToStr((C as TButton).TabOrder);
      St := St + ' NAME=' + (C as TButton).Name;
      if not (C as TButton).Enabled then
        St := St + ' DISABLED';
      St := St + '>';
      St := St + (C as TButton).Caption + '</BUTTON>';
    end
    else
    if C is TMemo then
    begin
      St := Format('<TEXTAREA style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TMemo).Left, (C as TMemo).Top, (C as TMemo).Height, (C as TMemo).Width]);
      St := St + FontToCss((C as TMemo).Font) + '"';
      St := St + ' TITLE="' + (C as TMemo).Hint + '"';
      if (C as TMemo).ReadOnly then
        St := St + ' ReadOnly';
      if not (C as TMemo).Enabled then
        St := St + ' DISABLED';
      St := St + ' NAME=' + (C as TMemo).Name;
      St := St + ' TABORDER=' + IntToStr((C as TMemo).TabOrder);
      if (C as TMemo).WordWrap then
        St := St + ' WRAP=PHYSICAL'
      else
        St := St + ' WRAP=OFF';
      St := St + '>';
      St := St + (C as TMemo).Text + '</TEXTAREA>';
    end
    else
    if C is TCheckBox then
    begin
      St := Format('<INPUT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TCheckBox).Left, (C as TCheckBox).Top, (C as TCheckBox).Height, 10]);
      St := St + FontToCss((C as TCheckBox).Font) + '"';
      St := St + ' TITLE="' + (C as TCheckBox).Hint + '"';
      if not (C as TCheckBox).Enabled then
        St := St + ' DISABLED';
      if (C as TCheckBox).Checked then
        St := St + ' CHECKED';
      St := St + ' TABORDER=' + IntToStr((C as TCheckBox).TabOrder);
      St := St + ' NAME=' + (C as TCheckBox).Name;
      St := St + ' TYPE="CHECKBOX">';
      HTML.Add(St);
      St := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d', [(C as TCheckBox).Left + 13, (C
          as TCheckBox).Top, (C as TCheckBox).Height, (C as TCheckBox).Width]);
      St := St + FontToCss((C as TCheckBox).Font) + '"';
      St := St + ' TITLE="' + (C as TCheckBox).Hint + '"';
      St := St + '>';
      St := St + (C as TCheckBox).Caption + '</LABEL>';
    end
    else
    if C is TRadioButton then
    begin
      St := Format('<INPUT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TRadioButton).Left, (C as TRadioButton).Top, (C as TRadioButton).Height, 10]);
      St := St + FontToCss((C as TRadioButton).Font) + '"';
      St := St + ' TITLE="' + (C as TRadioButton).Hint + '"';
      if not (C as TRadioButton).Enabled then
        St := St + ' DISABLED';
      if (C as TRadioButton).Checked then
        St := St + ' CHECKED';
      St := St + ' NAME=' + (C as TRadioButton).Parent.Name;
      St := St + ' TABORDER=' + IntToStr((C as TRadioButton).TabOrder);
      St := St + ' TYPE="RADIO">';
      HTML.Add(St);
      St := Format('<LABEL style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TRadioButton).Left + 13, (C as TRadioButton).Top, (C as TRadioButton).Height, (C as
          TRadioButton).Width]);
      St := St + FontToCss((C as TRadioButton).Font) + '"';
      St := St + ' TITLE="' + (C as TRadioButton).Hint + '"';
      St := St + '>';
      St := St + (C as TRadioButton).Caption + '</LABEL>';
    end
    else
    if C is TEdit then
    begin
      St := Format('<INPUT TYPE="TEXT" style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TEdit).Left, (C as TEdit).Top, (C as TEdit).Height, (C as TEdit).Width]);
      St := St + FontToCss((C as TEdit).Font) + '"';
      St := St + ' TITLE="' + (C as TEdit).Hint + '"';
      St := St + ' TABORDER=' + IntToStr((C as TEdit).TabOrder);
      St := St + ' NAME=' + (C as TEdit).Name;
      if (C as TEdit).ReadOnly then
        St := St + ' ReadOnly';
      if (C as TEdit).MaxLength <> 0 then
        St := St + ' MAXLENGTH=' + IntToStr((C as TEdit).MaxLength);
      if not (C as TEdit).Enabled then
        St := St + ' DISABLED';
      St := St + ' Value=' + (C as TEdit).Text;
      St := St + '>';
    end
    else
    if C is TComboBox then
    begin
      St := Format('<SELECT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TComboBox).Left, (C as TComboBox).Top, (C as TComboBox).Height, (C as TComboBox).Width]);
      St := St + FontToCss((C as TComboBox).Font) + '"';
      St := St + ' TITLE="' + (C as TComboBox).Hint + '"';
      St := St + ' TABORDER=' + IntToStr((C as TComboBox).TabOrder);
      St := St + ' NAME=' + (C as TComboBox).Name;
      if not (C as TComboBox).Enabled then
        St := St + ' DISABLED';
      St := St + '>';
      HTML.Add(St);
      for J := 0 to (C as TComboBox).Items.Count - 1 do
      begin
        if (C as TComboBox).ItemIndex = J then
          HTML.Add('<OPTION SELECTED>' + (C as TComboBox).Items[J])
        else
          HTML.Add('<OPTION>' + (C as TComboBox).Items[J]);
      end;
      St := '</SELECT>';
    end
    else
    if C is TListBox then
    begin
      St := Format('<SELECT style="position:absolute;Left:%d;Top:%d;Height:%d;Width:%d',
        [(C as TListBox).Left, (C as TListBox).Top, (C as TListBox).Height, (C as TListBox).Width]);
      St := St + FontToCss((C as TListBox).Font) + '"';
      St := St + ' MULTIPLE TITLE="' + (C as TListBox).Hint + '"';
      St := St + ' TABORDER=' + IntToStr((C as TListBox).TabOrder);
      St := St + ' NAME=' + (C as TListBox).Name;
      if not (C as TListBox).Enabled then
        St := St + ' DISABLED';
      St := St + '>';
      HTML.Add(St);
      for J := 0 to (C as TListBox).Items.Count - 1 do
      begin
        if (C as TListBox).ItemIndex = J then
          HTML.Add('<OPTION SELECTED>' + (C as TListBox).Items[J])
        else
          HTML.Add('<OPTION>' + (C as TListBox).Items[J]);
      end;
      St := '</SELECT>';
    end;

    if St <> '' then
      HTML.Add(St);
  end;
  HTML.Add('</BODY></HTML>');
  HTML.SaveToFile(Filename);
  HTML.Free;
end;

end.

