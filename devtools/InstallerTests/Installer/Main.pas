unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CheckLst, D5CheckLst, DelphiData, ExtCtrls,
  Buttons, ConfigurationBase, JvComponentBase, JvComCtrls, JvTabBar,
  JvExControls, JvComponent, JvPageList;

type
  TFormMain = class(TForm)
    clbPersonalities: TCheckListBox;
    pnlClient: TPanel;
    pnlTitle: TPanel;
    lblTitle: TLabel;
    lblSubTitle: TLabel;
    pnlContent: TPanel;
    pglOptions: TJvPageList;
    tbrOptions: TJvTabBar;
    spgEmpty: TJvStandardPage;
    procedure clbPersonalitiesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clbPersonalitiesClick(Sender: TObject);
    procedure clbPersonalitiesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }

    FDelphiIDEList: TDelphiIDEList;
    FSupportedPersonalityOptions: TObjectList;

    procedure GenerateTabs(PersoOption: TPersonalityOptions);
    procedure ShowOptions(PersoOption: TPersonalityOptions);
  public
    { Public-Deklarationen }
    property DelphiIDEList: TDelphiIDEList read FDelphiIDEList;
  end;

var
  FormMain: TFormMain;

implementation

uses
  InstallerConsts, ConfigOptions, Utils;

{$R *.dfm}
{$R winxp.res}

procedure TFormMain.FormCreate(Sender: TObject);
var
  IDEIndex, PersIndex: Integer;
  SupportedPersonalities: TList;
  UpToDate: Boolean;
  PersoOptions: TPersonalityOptions;
begin
  FDelphiIDEList := TDelphiIDEList.Create;
  FSupportedPersonalityOptions := TObjectList.Create;

  Caption := Format(RsInstallerCaption, [Configuration.Name, Configuration.VersionStr]);
  Application.Title := Caption;
  Application.HintHidePause := MaxInt;

  // build supported personality list
  clbPersonalities.HeaderBackgroundColor := clBtnFace;
  SupportedPersonalities := TList.Create;
  try
    for IDEIndex := 0 to FDelphiIDEList.Count - 1 do
    begin
      with FDelphiIDEList[IDEIndex] do
      begin
        SupportedPersonalities.Clear;
        for PersIndex := 0 to PersonalityCount - 1 do
          if Configuration.SupportsPersonality(Personalities[PersIndex]) then
            SupportedPersonalities.Add(Personalities[PersIndex]);

        if SupportedPersonalities.Count > 0 then
        begin
          UpToDate := Configuration.IsUpToDate(FDelphiIDEList[IDEIndex]);
          clbPersonalities.Header[clbPersonalities.Items.AddObject(DisplayName, Pointer(UpToDate))] := True;
          if UpToDate then
          begin
            for PersIndex := 0 to SupportedPersonalities.Count - 1 do
            begin
              PersoOptions := Configuration.CreateOptionsFor(SupportedPersonalities[PersIndex]);
              FSupportedPersonalityOptions.Add(PersoOptions);
              clbPersonalities.Items.AddObject(TIDEPersonality(SupportedPersonalities[PersIndex]).Name,
                                               PersoOptions);
              clbPersonalities.Checked[clbPersonalities.Items.Count - 1] := PersoOptions.Install;
              GenerateTabs(PersoOptions);
            end;
          end;
        end;
      end;
    end;
  finally
    SupportedPersonalities.Free;
  end;
  clbPersonalitiesClick(clbPersonalities);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FSupportedPersonalityOptions.Free;
  FDelphiIDEList.Free;
end;

procedure TFormMain.clbPersonalitiesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  clb: TCheckListBox;
begin
  clb := Control as TCheckListBox;
  if clb.Header[Index] then
  begin
    if not Boolean(clb.Items.Objects[Index]) then
      clb.Canvas.Font.Color := clRed;
    clb.Canvas.Font.Style := [fsBold]
  end
  else
    clb.Canvas.Font.Style := [];
  clb.Canvas.FillRect(Rect);
  clb.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, clb.Items[Index]);
  if (Index > 0) and clb.Header[Index] then
  begin
    clb.Canvas.Pen.Color := clGray;
    clb.Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Top + 1);
  end;
end;

procedure TFormMain.clbPersonalitiesClick(Sender: TObject);
var
  PersoOptions: TPersonalityOptions;
  clb: TCheckListBox;
begin
  clb := Sender as TCheckListBox;
  if clb.ItemIndex = -1 then
  begin
    lblTitle.Caption := RsNoIdeOrPersonaltySelected;
    lblSubTitle.Caption := '';
    ShowOptions(nil);
  end
  else
  begin
    if clb.Header[clb.ItemIndex] then
    begin
      lblTitle.Caption := clb.Items[clb.ItemIndex];
      if Boolean(clb.Items.Objects[clb.ItemIndex]) then
      begin
        if (clb.ItemIndex < clb.Items.Count - 1) and
           not clb.Header[clb.ItemIndex + 1] then
        begin
          // select first personality
          clb.ItemIndex := clb.ItemIndex + 1;
          clbPersonalitiesClick(clbPersonalities);
        end
        else
        begin
          lblSubTitle.Caption := '';
          ShowOptions(nil);
        end;
      end
      else
      begin
        lblSubTitle.Caption := RsIdeNotUpToDate;
        ShowOptions(nil);
      end;
    end
    else
    begin
      PersoOptions := TPersonalityOptions(clb.Items.Objects[clb.ItemIndex]);
      lblTitle.Caption := PersoOptions.Personality.IDE.DisplayName;
      lblSubTitle.Caption := PersoOptions.Personality.Name;

      ShowOptions(PersoOptions);
    end;
  end;
end;

procedure TFormMain.clbPersonalitiesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  clb: TCheckListBox;
begin
  clb := Sender as TCheckListBox;
  if Key = VK_UP then
  begin
    if (clb.ItemIndex > 1) and clb.Header[clb.ItemIndex - 1] then
    begin
      clb.ItemIndex := clb.ItemIndex - 2;
      clbPersonalitiesClick(clb);
      Key := 0;
    end;
  end;
end;

procedure TFormMain.GenerateTabs(PersoOption: TPersonalityOptions);
var
  TabIndex, OptionIndex: Integer;
  Page: TJvStandardPage;
  TabItem: TJvTabBarItem;
  ScrollBox: TScrollBox;
  Options: TOptionList;
  Control: TControl;
begin
  if Assigned(PersoOption) then
  begin
    for TabIndex := 0 to PersoOption.TabNames.Count - 1 do
    begin
      TabItem := tbrOptions.AddTab(PersoOption.TabNames[TabIndex]);
      TabItem.Visible := False;
      Page := TJvStandardPage.Create(nil);
      TabItem.AutoDeleteData.Add(Page);
      Page.Data := TabItem;
      Page.Caption := TabItem.Caption;
      Page.BorderWidth := 6;
      Page.Visible := False;

      ScrollBox := TScrollBox.Create(Page);
      ScrollBox.Name := 'ScrollBoxClient';
      ScrollBox.Align := alClient;
      ScrollBox.BorderStyle := bsNone;
      ScrollBox.Parent := Page;
      {$IFDEF CONDITIONALEXPRESSIONS}
      {$IFNDEF VER140}
      ScrollBox.ParentBackground := True;
      {$ENDIF !VER140}
      {$ENDIF CONDITIONALEXPRESSIONS}
      Page.PageList := pglOptions;

      Options := PersoOption.GetTabOptions(TabIndex);
      TabItem.Data := PersoOption;
      Page.Tag := Integer(Options);

      if Options <> nil then
      begin
        for OptionIndex := 0 to Options.Count - 1 do
        begin
          Control := Options[OptionIndex].CreateControl;
          if Control <> nil then
          begin
            Control.Align := alTop;
            Control.Top := MaxInt;
            ScrollBox.InsertComponent(Control);
            Control.Parent := ScrollBox;
          end;
        end;
      end;
    end;
    for TabIndex := pglOptions.PageCount - 1 downto 0 do
      if pglOptions.Pages[TabIndex].Tag = 0 then
        pglOptions.Pages[TabIndex].Data.Free;
  end;
  spgEmpty.PageIndex := pglOptions.PageCount;
end;

procedure TFormMain.ShowOptions(PersoOption: TPersonalityOptions);
var
  i: Integer;
  SelTab: string;
begin
  if tbrOptions.Tag <> Integer(PersoOption) then
  begin
    for i := 0 to tbrOptions.Tabs.Count - 1 do
      TJvCustomPage(tbrOptions.Tabs[i].AutoDeleteData[0]).Visible := tbrOptions.Tabs[i].Data = PersoOption;
  end;
end;

end.
