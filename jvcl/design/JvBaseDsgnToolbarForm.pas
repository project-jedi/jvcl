unit JvBaseDsgnToolbarForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvBaseDsgnForm, ImgList, ToolWin, ComCtrls, ActnList, ExtCtrls, Menus;

type
  TJvBaseDesignToolbar = class(TJvBaseDesign)
    tbrToolbar: TToolBar;
    ilToolbar: TImageList;
    spToolbar: TSplitter;
    pmToolbar: TPopupMenu;
    miTextLabels: TMenuItem;
    aiToolbar: TActionList;
    aiTextLabels: TAction;
    aiShowToolbar: TAction;
    procedure aiTextLabelsExecute(Sender: TObject);
    procedure aiShowToolbarExecute(Sender: TObject);
    procedure spToolbarCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
  private
    { Private declarations }
  protected
    procedure StoreSettings; override;
    procedure RestoreSettings; override;
    procedure EnableLargeButtons(Value: Boolean); dynamic;
    procedure ShowToolbar(Value: Boolean); dynamic;
  public
    { Public declarations }
  end;

var
  JvBaseDesignToolbar: TJvBaseDesignToolbar;

implementation

uses
  Registry;

{$R *.DFM}

procedure TJvBaseDesignToolbar.StoreSettings;
begin
  inherited StoreSettings;
  with TRegistry.Create do
  try
    LazyWrite := False;
    if OpenKey(GetRegKey, True) then
    try
      WriteBool('LargeButton', aiTextLabels.Checked);
      WriteBool('Toolbar', aiShowToolbar.Checked);
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TJvBaseDesignToolbar.RestoreSettings;
begin
  inherited RestoreSettings;
  with TRegistry.Create do
  try
    if OpenKey(GetRegKey, False) then
    try
      if ValueExists('LargeButton') then
        EnableLargeButtons(ReadBool('LargeButton'));
      if ValueExists('Toolbar') then
        ShowToolbar(ReadBool('Toolbar'));
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
end;

procedure TJvBaseDesignToolbar.EnableLargeButtons(Value: Boolean);
begin
  tbrToolbar.ShowCaptions := Value;
  aiTextLabels.Checked := Value;
  if not Value then
  begin
    tbrToolbar.ShowCaptions := False;
    tbrToolbar.ButtonWidth := 22;
    tbrToolbar.ButtonHeight := 22;
  end;
end;

procedure TJvBaseDesignToolbar.ShowToolbar(Value: Boolean);
begin
  aiShowToolbar.Checked := Value;
  spToolbar.Visible := Value;
  tbrToolbar.Visible := Value;
end;

procedure TJvBaseDesignToolbar.aiTextLabelsExecute(Sender: TObject);
begin
  EnableLargeButtons(not aiTextLabels.Checked);
end;

procedure TJvBaseDesignToolbar.aiShowToolbarExecute(Sender: TObject);
begin
  ShowToolbar(not aiShowToolbar.Checked);
end;

procedure TJvBaseDesignToolbar.spToolbarCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if NewSize > 36 then
    NewSize := 44
  else
    NewSize := 30;
  Accept := True;
  EnableLargeButtons(NewSize = 44);
end;

end.
