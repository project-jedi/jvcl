{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReportParamsEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgReportParamsEditor;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Buttons, ExtCtrls, Mask,
  JvgStringGrid, JvgReport;

type
  TJvgReportParamsEditor = class(TComponent)
  private
    FReport: TJvgReport;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Edit;
  published
    property Report: TJvgReport read FReport write FReport;
  end;

implementation

procedure TJvgReportParamsEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Report) and (Operation = opRemove) then
    Report := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvgReportParamsEditor.Edit;
// var
//  Form: TJvgReportParamsForm;
//  Label_: TLabel;
//  Edit_: TCustomEdit;
//  Check_: TCheckBox;
//  Radio_: TRadioGroup;
//  SList: TStringList;
//  y, x, i, j, EditLeft, LastHeight: Integer;
begin //temporary commented
  {
    if Report = nil then
      Exit;

    if Report.ParamNames.Count = 0 then
      for i := 0 to Report.ReportText.Count - 1 do
        Report.AnalyzeParams(Report.ReportText[i], '');

    Form := TJvgReportParamsForm.Create(nil);
    y := 4;
    EditLeft := 0;
    SList := nil;
    try
      for i := 0 to Report.ParamNames.Count-1 do
      with Report do
      begin
        case TglRepParamType(ParamTypes[i]) of
          gptEdit:
          begin
            Label_ := TLabel.Create(Form);
            try
              if StrToInt(ParamMasks[i]) > 50 then
                Edit_ := TMemo.Create(Form);
              TMemo(Edit_).Height := Form.Canvas.TextHeight('x') * StrToInt(ParamMasks[i]) div 50;
            except
              Edit_ := TMaskEdit.Create(Form);
            end;
            Label_.Parent := Form.SB;
            Label_.Left := 3;
            Label_.Top := y;
            Label_.Caption := ParamNames[i];
            x := Form.Canvas.TextWidth(ParamNames[i]);
            Edit_.Parent := Form.SB;
            Edit_.Left := x + 6;
            Edit_.Top := y;
            Edit_.Text := ParamValues[i];
            try
              if StrToInt(ParamMasks[i])>50 then
                Edit_.Width := Form.Canvas.TextWidth('x')* 50
              else
                Edit_.Width := Form.Canvas.TextWidth('x')*StrToInt(ParamMasks[i]);
            except
              TMaskEdit(Edit_).EditMask := ParamMasks[i];
              if Edit_.Width < Form.Canvas.TextWidth('x')*Length(ParamMasks[i]) then
                Edit_.Width := Form.Canvas.TextWidth('x')*Length(ParamMasks[i]);
            end;
            if Edit_.Left > EditLeft then
              EditLeft := Edit_.Left;
            Inc(x, Edit_.Width);
            LastHeight := Edit_.Height;
          end;
          gptRadio:
          begin
            Radio_ := TRadioGroup.Create(Form);
            Radio_.Parent := Form.SB;
            Radio_.Left := 3;
            Radio_.Top := y;
            if not Assigned(SList) then
              SList := TStringList.Create;
            SList.CommaText := ParamNames[i];
            Radio_.Caption := SList[0];
            for j := 1 to SList.Count-1 do
              Radio_.Items.Add(SList[j]);

            Radio_.Height := (Form.Canvas.TextHeight('ly')+3) * SList.Count-1;
            LastHeight := Radio_.Height;
          end;
          gptCheck:
          begin
            Check_ := TCheckBox.Create(Form);
            Check_.Parent := Form.SB;
            Check_.Left := 3;
            Check_.Top := y;
            Check_.Caption := ParamNames[i];
            LastHeight := Check_.Height;
          end;
        end;

        if x+10 >= Form.ClientWidth then
          Form.ClientWidth := x + 10;
        Inc(y, LastHeight);
      end;

      for i := 0 to Form.SB.ControlCount-1 do
        if Form.SB.Controls[i] is TMaskEdit then
          TMaskEdit(Form.SB.Controls[i]).Left := EditLeft;

      Form.ClientHeight := Form.Panel1.Height + y + 10;

      Form.ShowModal;
    finally
      SList.Free;
      Form.Free;
    end;
    }
end;

end.
