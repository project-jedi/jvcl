{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvFooterEditor;

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
  TJvFooterEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation
uses
  JvTypes, JvFooter;

//=== TJvFooterEditor ========================================================

function TJvFooterEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

function TJvFooterEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Add button';
    1:
      Result := '-';
    2:
      Result := 'MS Office 2000';
    3:
      Result := 'MS Enterprise Manager Wizard';
    4:
      Result := 'Dialog Mode';
  end;
end;

procedure TJvFooterEditor.ExecuteVerb(Index: Integer);
var
  FButton: TJvFooterBtn;
begin
  case Index of
    0:
      Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50);
    1:
      ;
    2:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Help';
        FButton.Alignment := taLeftJustify;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'OK';
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Cancel';
        FButton.Cancel := True;
      end;
    3:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Previous';
        FButton.SpaceInterval := 0;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Next';
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'Close';
      end;
    4:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := 'OK';
        FButton.SpaceInterval := 0;
        FButton.Alignment := taCenter;
      end;
  end;
end;

procedure TJvFooterEditor.Edit;
begin
  // We don't need to add band on double click
end;
  

end.
