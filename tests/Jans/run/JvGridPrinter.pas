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
{$I JEDI.INC}
unit JvGridPrinter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, printers;

type
  TPrintMode = (pmPrint, pmPreview, pmPageCount);

  TPrintOptions = class(TPersistent)
  private
    fJobTitle: string;
    fPageTitle: string;
    fPageTitleMargin: Cardinal;
    fCopies: Cardinal;
    fPreviewPage: Cardinal;
    fBorderStyle: TBorderStyle;
    fLeftPadding: Cardinal;
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
  protected
  public

  private
  published
    property Orientation: TPrinterOrientation read FOrientation write SetOrientation;
    property JobTitle: string read fJobTitle write fJobTitle;
    property PageTitle: string read fPageTitle write fPageTitle;
    property Logo: string read FLogo write SetLogo;
    property PageTitleMargin: Cardinal read fpageTitleMargin write fpageTitleMargin;
    property PageFooter: string read FPageFooter write SetPageFooter;
    property HeaderSize: Cardinal read FHeaderSize write SetHeaderSize;
    property FooterSize: Cardinal read FFooterSize write SetFooterSize;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property Copies: Cardinal read fCopies write fCopies default 1;
    property PreviewPage: Cardinal read fPreviewPage write fPreviewPage;
    property BorderStyle: TBorderstyle read fBorderStyle write fBorderStyle;
    property Leftpadding: Cardinal read fLeftpadding write fLeftpadding;
    property MarginBottom: Cardinal read FMarginBottom write SetMarginBottom;
    property MarginLeft: Cardinal read FMarginLeft write SetMarginLeft;
    property MarginTop: Cardinal read FMarginTop write SetMarginTop;
    property MarginRight: Cardinal read FMarginRight write SetMarginRight;
  end;

  TJvGridPrinter = class(TComponent)
  private
    { Private declarations }
    FPrintOptions: TPrintOptions;
    FGrid: TStringGrid;
    FNumbersalRight: boolean;
    FNumberFormat: string;
    FWordwrap: boolean;
    procedure SetGrid(const Value: TStringGrid);
    procedure SetNumbersalRight(const Value: boolean);
    procedure SetNumberFormat(const Value: string);
    procedure SetWordwrap(const Value: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Preview: boolean;
  published
    { Published declarations }
    property PrintOptions: TPrintOptions read fPrintOptions write fPrintOptions;
    property Grid: TStringGrid read FGrid write SetGrid;
    property Wordwrap: boolean read FWordwrap write SetWordwrap;
    property NumbersalRight: boolean read FNumbersalRight write SetNumbersalRight;
    property NumberFormat: string read FNumberFormat write SetNumberFormat;
  end;

implementation

uses
  JvGridPreviewForm;

const
  cr = chr(13) + chr(10);
  tab = chr(9);

  { TPrintOptions }

procedure TPrintOptions.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
end;

procedure TPrintOptions.SetFooterSize(const Value: Cardinal);
begin
  FFooterSize := Value;
end;

procedure TPrintOptions.SetHeaderSize(const Value: Cardinal);
begin
  FHeaderSize := Value;
end;

procedure TPrintOptions.SetLogo(const Value: string);
begin
  FLogo := Value;
end;

procedure TPrintOptions.SetMarginBottom(const Value: Cardinal);
begin
  FMarginBottom := Value;
end;

procedure TPrintOptions.SetMarginLeft(const Value: Cardinal);
begin
  FMarginLeft := Value;
end;

procedure TPrintOptions.SetMarginRight(const Value: Cardinal);
begin
  FMarginRight := Value;
end;

procedure TPrintOptions.SetMarginTop(const Value: Cardinal);
begin
  FMarginTop := Value;
end;

procedure TPrintOptions.SetOrientation(const Value: TPrinterOrientation);
begin
  FOrientation := Value;
end;

procedure TPrintOptions.SetPageFooter(const Value: string);
begin
  FPageFooter := Value;
end;

procedure TPrintOptions.SetTimeFormat(const Value: string);
begin
  FTimeFormat := Value;
end;

{ TJvGridPrinter }

constructor TJvGridPrinter.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FPrintOptions := TPrintOptions.Create;
  FPrintOptions.PageFooter := 'date|time|page';
  FPrintOptions.DateFormat := 'd-mmm-yyyy';
  FprintOptions.TimeFormat := 'h:nn am/pm';
  FprintOptions.HeaderSize := 14;
  FPrintOptions.FooterSize := 8;
  FprintOptions.PreviewPage := 1;
  FNumbersalright := true;
  FNumberFormat := '%.2f';
  FWordwrap := true;
end;

destructor TJvGridPrinter.Destroy;
begin
  FPrintOptions.free;
  inherited destroy;
end;

function TJvGridPrinter.Preview: boolean;
var
  preview: TJvGridPreviewF;
begin
  if assigned(FGrid) then
  begin
    preview := TJvGridPreviewF.create(application);
    preview.GridPrinter := self;
    preview.Grid := Grid;
    preview.ShowModal;
    preview.free;
    result := true;
  end
  else
    result := false;
end;

procedure TJvGridPrinter.SetNumbersalRight(const Value: boolean);
begin
  FNumbersalRight := Value;
end;

procedure TJvGridPrinter.SetNumberFormat(const Value: string);
begin
  FNumberFormat := Value;
end;

procedure TJvGridPrinter.SetGrid(const Value: TStringGrid);
begin
  FGrid := Value;
end;

procedure TJvGridPrinter.SetWordwrap(const Value: boolean);
begin
  FWordwrap := Value;
end;

end.
