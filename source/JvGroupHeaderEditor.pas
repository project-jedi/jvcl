{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvGroupHeaderEditor;

interface
uses
  Windows, Forms, Graphics, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes, Dialogs, Controls;

type
  TJvGroupHeaderEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;


implementation
uses
  JvTypes, JvGroupHeader;

//=== TJvGroupHeaderEditor ===================================================

function TJvGroupHeaderEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TJvGroupHeaderEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Standard/Flat';
    1:
      Result := 'Web';
  end;
end;

procedure TJvGroupHeaderEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      with TJvGroupHeader(Component) do
      begin
        BevelOptions.Style := bsLowered;
        Font.Style := [];
      end;
    1:
      with TJvGroupHeader(Component) do
      begin
        BevelOptions.Style := bsShape;
        BevelOptions.Brush.Color := $00A97A1B;
        BevelOptions.Pen.Color := $00E1AD40;
        BevelOptions.Height := 3;
        Font.Style := [fsBold];
      end;
  end;
end;

procedure TJvGroupHeaderEditor.Edit;
begin
  // We don't need to add band on double click
end;


end.
