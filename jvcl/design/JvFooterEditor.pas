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


resourcestring
  sAddButton = 'Add button';
  sMSOffice = 'MS Office 2000';
  sMSEnterpriseManagerWizard = 'MS Enterprise Manager Wizard';
  sDialogMode = 'Dialog Mode';
  sPrevious = 'Previous';
  sNext = 'Next';

implementation
uses
  Consts, 
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
      Result := sAddButton;
    1:
      Result := '-'; // do not localize
    2:
      Result := sMSOffice;
    3:
      Result := sMSEnterpriseManagerWizard;
    4:
      Result := sDialogMode;
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
        FButton.Caption := SHelpButton;
        FButton.Alignment := taLeftJustify;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := SOKButton;
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := SCancelButton;
        FButton.Cancel := True;
      end;
    3:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := sPrevious;
        FButton.SpaceInterval := 0;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := sNext;
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := SCloseButton;
      end;
    4:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50));
        FButton.Caption := SOKButton;
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
