{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPrint.PAS, released on 2001-02-28.

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

unit JvPrint;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Printers, JvTypes, JvComponent;

type
  TJvPrint = class(TJvComponent)
  private
    FOnBegin: TNotifyEvent;
    FOnEnded: TNotifyEvent;
    FOnProgress: TOnPrnProgress;
    FOnNextP: TOnNextPage;
  published
    procedure Print(Value: TStringList);
    procedure PrintImage(Value: TBitmap; Style: TBitmapStyle);
    procedure Abort;
    property OnBeginPrint: TNotifyEvent read FOnBegin write FOnBegin;
    property OnFinishedPrint: TNotifyEvent read FOnEnded write FOnEnded;
    property OnProgress: TOnPrnProgress read FOnProgress write FOnProgress;
    property OnNextPage: TOnNextPage read FOnNextP write FOnNextP;
  end;

implementation

{**************************************************}

procedure TJvPrint.Abort;
begin
  Printer.Abort;
end;

{**************************************************}

procedure TJvPrint.Print(Value: TStringList);
var
  I, Line, Pagenum: Integer;
begin
  //let's print
  if Assigned(FOnBegin) then
    FOnBegin(Self);
  line := 0;
  Printer.BeginDoc;
  Pagenum := 1;
  for i := 0 to Value.Count - 1 do
  begin
    if Assigned(FOnProgress) then
      FOnProgress(Self, i + 1, Value.Count);

    Line := Line + Printer.Canvas.TextHeight(Value[i]);
    if Line + Printer.Canvas.TextHeight(Value[i]) > Printer.PageHeight then
    begin
      Line := Printer.Canvas.TextHeight(Value[i]);
      Printer.NewPage;
      Inc(PageNum);
      if Assigned(FonNextP) then
        FonNextP(Self, PageNum);
    end;

    Printer.Canvas.TextOut(0, Line, Value[I]);
  end;
  Printer.EndDoc;
  if Assigned(FOnEnded) then
    FOnEnded(Self);
end;

{**************************************************}

procedure TJvPrint.PrintImage(Value: TBitmap; Style: TBitmapStyle);
begin
  //let's print too :)
  if Assigned(FOnBegin) then
    FOnBegin(Self);
  case Style of
    bsNormal:
      begin
        with Printer do
        begin
          BeginDoc;
          Canvas.Draw(0, 0, Value);
          EndDoc;
        end;
      end;
    bsCentered:
      begin
        with Printer do
        begin
          BeginDoc;
          Canvas.Draw((PageWidth - Value.Width) div 2, (PageHeight - Value.Height) div 2, Value);
          EndDoc;
        end;
      end;
    bsStretched:
      begin
        with Printer do
        begin
          BeginDoc;
          Canvas.StretchDraw(Rect(0, 0, PageWidth, PageHeight), Value);
          EndDoc;
        end;
      end;
  end;
  if Assigned(FOnEnded) then
    FOnEnded(Self);
end;

end.
