{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGridPrinter.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvGridPrinter;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  Grids, Printers;

type
  TJvPrintMode = (pmPrint, pmPreview, pmPageCount);

  TJvPrintOptions = class(TPersistent)
  private
    FJobTitle: string;
    FPageTitle: string;
    FPageTitleMargin: Cardinal;
    FCopies: Cardinal;
    FPreviewPage: Cardinal;
    FBorderStyle: TBorderStyle;
    FLeftPadding: Cardinal;
    FMarginBottom: Cardinal;
    FMarginLeft: Cardinal;
    FMarginTop: Cardinal;
    FMarginRight: Cardinal;
    FPageFooter: string;
    FDateFormat: string;
    FTimeFormat: string;
    FHeaderSize: Cardinal;
    FFooterSize: Cardinal;
    FOrientation: TPrinterOrientation;
    FLogo: string;
    procedure SetMarginBottom(const Value: Cardinal);
    procedure SetMarginLeft(const Value: Cardinal);
    procedure SetMarginTop(const Value: Cardinal);
    procedure SetMarginRight(const Value: Cardinal);
    procedure SetPageFooter(const Value: string);
    procedure SetDateFormat(const Value: string);
    procedure SetTimeFormat(const Value: string);
    procedure SetFooterSize(const Value: Cardinal);
    procedure SetHeaderSize(const Value: Cardinal);
    procedure SetOrientation(const Value: TPrinterOrientation);
    procedure SetLogo(const Value: string);
  published
    property Orientation: TPrinterOrientation read FOrientation write SetOrientation;
    property JobTitle: string read FJobTitle write FJobTitle;
    property PageTitle: string read FPageTitle write FPageTitle;
    property Logo: string read FLogo write SetLogo;
    property PageTitleMargin: Cardinal read FPageTitleMargin write FPageTitleMargin;
    property PageFooter: string read FPageFooter write SetPageFooter;
    property HeaderSize: Cardinal read FHeaderSize write SetHeaderSize;
    property FooterSize: Cardinal read FFooterSize write SetFooterSize;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property Copies: Cardinal read FCopies write FCopies default 1;
    property PreviewPage: Cardinal read FPreviewPage write FPreviewPage;
    property BorderStyle: TBorderstyle read FBorderStyle write FBorderStyle;
    property Leftpadding: Cardinal read FLeftPadding write FLeftPadding;
    property MarginBottom: Cardinal read FMarginBottom write SetMarginBottom;
    property MarginLeft: Cardinal read FMarginLeft write SetMarginLeft;
    property MarginTop: Cardinal read FMarginTop write SetMarginTop;
    property MarginRight: Cardinal read FMarginRight write SetMarginRight;
  end;

  TJvGridPrinter = class(TComponent)
  private
    FPrintOptions: TJvPrintOptions;
    FGrid: TStringGrid;
    FNumbersAlright: Boolean;
    FNumberFormat: string;
    FWordWrap: Boolean;
    procedure SetGrid(const Value: TStringGrid);
    procedure SetNumbersAlright(const Value: Boolean);
    procedure SetNumberFormat(const Value: string);
    procedure SetWordWrap(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Preview: Boolean;
  published
    property PrintOptions: TJvPrintOptions read FPrintOptions write FPrintOptions;
    property Grid: TStringGrid read FGrid write SetGrid;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    property NumbersAlright: Boolean read FNumbersAlright write SetNumbersAlright default True;
    property NumberFormat: string read FNumberFormat write SetNumberFormat;
  end;

implementation

uses
  JvGridPreviewForm, JvTypes;

resourcestring
  SPrintOptionsPageFooter = 'date|time|page';
  SPrintOptionsDateFormat = 'd-mmm-yyyy';
  SPrintOptionsTimeFormat = 'h:nn am/pm';

//=== TJvPrintOptions ========================================================

procedure TJvPrintOptions.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
end;

procedure TJvPrintOptions.SetFooterSize(const Value: Cardinal);
begin
  FFooterSize := Value;
end;

procedure TJvPrintOptions.SetHeaderSize(const Value: Cardinal);
begin
  FHeaderSize := Value;
end;

procedure TJvPrintOptions.SetLogo(const Value: string);
begin
  FLogo := Value;
end;

procedure TJvPrintOptions.SetMarginBottom(const Value: Cardinal);
begin
  FMarginBottom := Value;
end;

procedure TJvPrintOptions.SetMarginLeft(const Value: Cardinal);
begin
  FMarginLeft := Value;
end;

procedure TJvPrintOptions.SetMarginRight(const Value: Cardinal);
begin
  FMarginRight := Value;
end;

procedure TJvPrintOptions.SetMarginTop(const Value: Cardinal);
begin
  FMarginTop := Value;
end;

procedure TJvPrintOptions.SetOrientation(const Value: TPrinterOrientation);
begin
  FOrientation := Value;
end;

procedure TJvPrintOptions.SetPageFooter(const Value: string);
begin
  FPageFooter := Value;
end;

procedure TJvPrintOptions.SetTimeFormat(const Value: string);
begin
  FTimeFormat := Value;
end;

//=== TJvGridPrinter =========================================================

constructor TJvGridPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrintOptions := TJvPrintOptions.Create;
  FPrintOptions.PageFooter := SPrintOptionsPageFooter;
  FPrintOptions.DateFormat := SPrintOptionsDateFormat;
  FPrintOptions.TimeFormat := SPrintOptionsTimeFormat;
  FPrintOptions.HeaderSize := 14;
  FPrintOptions.FooterSize := 8;
  FPrintOptions.PreviewPage := 1;
  FNumbersAlright := True;
  FNumberFormat := '%.2f';
  FWordWrap := True;
end;

destructor TJvGridPrinter.Destroy;
begin
  FPrintOptions.Free;
  inherited Destroy;
end;

function TJvGridPrinter.Preview: Boolean;
var
  Preview: TJvGridPreviewForm;
begin
  if Assigned(FGrid) then
  begin
    Preview := TJvGridPreviewForm.Create(Application);
    Preview.GridPrinter := Self;
    Preview.Grid := Grid;
    Preview.ShowModal;
    Preview.Free;
    Result := True;
  end
  else
    Result := False;
end;

procedure TJvGridPrinter.SetNumbersAlright(const Value: Boolean);
begin
  FNumbersAlright := Value;
end;

procedure TJvGridPrinter.SetNumberFormat(const Value: string);
begin
  FNumberFormat := Value;
end;

procedure TJvGridPrinter.SetGrid(const Value: TStringGrid);
begin
  FGrid := Value;
end;

procedure TJvGridPrinter.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

end.
