unit JvDocking_Test;

interface
uses
  Forms, Controls, ContNrs,
  Classes, TestFramework, JvDockControlForm, JvDockVSNetStyle, JvDockVIDStyle,
  JvDockSupportControl;

type
  TClientForm = class(TForm)
  private
    FDockClient: TJvDockClient;
    FOnFormShowCount: Integer;
    FOnFormHideCount: Integer;
    FOnShowCount: Integer;
    FOnHideCount: Integer;

    procedure DockClient1FormHide(Sender: TObject);
    procedure DockClient1FormShow(Sender: TObject);
    procedure HandleShow(Sender: TObject);
    procedure HandleHide(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure ResetCounters;

    property DockClient: TJvDockClient read FDockClient;
    property OnHideCount: Integer read FOnHideCount;
    property OnFormHideCount: Integer read FOnFormHideCount;
    property OnShowCount: Integer read FOnShowCount;
    property OnFormShowCount: Integer read FOnFormShowCount;
  end;

  TServerForm = class(TForm)
  private
    FDockVSNetStyle: TJvDockVSNetStyle;
    FDockVIDStyle: TJvDockVIDStyle;
    FDockServer: TJvDockServer;
    function GetDockVSNetStyle: TJvDockVSNetStyle;
    function GetDefaultStyle: TJvDockBasicStyle;
    function GetDockVIDStyle: TJvDockVIDStyle;
  public
    constructor Create(AOwner: TComponent); override;
    property DockVSNetStyle: TJvDockVSNetStyle read GetDockVSNetStyle;
    property DockVIDStyle: TJvDockVIDStyle read GetDockVIDStyle;
    property DockServer: TJvDockServer read FDockServer;
    property DefaultStyle: TJvDockBasicStyle read GetDefaultStyle;
  end;

  TJvDockingTestCase = class(TTestCase)
  private
    FLog: TStringList;
    FFormCount: Integer;
    function GetServerForm: TServerForm;
    procedure HandleServerFormDestroy(Sender: TObject);

    // log
    procedure ClearLog;
    procedure Log(const Msg: string);
    procedure LogFmt(const Format: string; const Args: array of const);
    procedure ShowLog;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    { Constructs a dockable client form }
    function ConstructClientForm: TClientForm;
    { Construct ACount dockable client forms }
    procedure ConstructClientList(AList: TList; const ACount: Integer);
    { Docks DropControl onto Target }
    procedure DoDock(Target, DropControl: TWinControl);

    procedure CheckShowHideDockForm_ShowAll(AList: TList);
    procedure CheckShowHideDockForm_HideAll(AList: TList);
    procedure CheckShowHideDockForm_ShowOne(AList: TList);
    procedure CheckShowHideDockForm_HideOne(AList: TList);
    procedure CheckShowHideDockForm_ShowTwo(AList: TList);
    procedure CheckShowHideDockForm_HideTwo(AList: TList);

    { Use CheckFormCount in conjunction with SaveFormCount, it checkes that the
      form count isn't changed after a (lengthy) operation. CheckFormCount basically
      checks whether all dock helper forms (TJvDockTabHostForm etc.) are cleaned up.
    }
    procedure SaveFormCount;
    procedure CheckFormCount;
    { Checks a ShowDockForm call }
    procedure CheckShowDockForm(AClientForm: TClientForm);
    { Checks a HideDockForm call }
    procedure CheckHideDockForm(AClientForm: TClientForm);
    procedure CheckFormVisible(const ExpValue: Boolean; AForm: TCustomForm);
    procedure CheckShowHideDockForm_2ClientForms(AClientForm1, AClientForm2: TClientForm);
    procedure CheckShowHideDockForm_ClientList(AList: TList);

    { We have 1 global server form that contains the style components, and where
      on the dock forms are docked if needed }
    property ServerForm: TServerForm read GetServerForm;
  published
    { Test freeing of docked forms, we don't call ProcessMessages between
      Free calls, but after all Free calls procedure Wait is called that calls
      processMessages multiple times
    }
    procedure Free_TabbedFloating;
    procedure Free_ConjoinedFloating;
    procedure Free_SingleDocked;
    procedure Free_RandomFloating;

    { Test freeing of docked forms, we don't call ProcessMessages between
      Release calls, but after all Release calls procedure Wait is called that calls
      processMessages multiple times
    }
    procedure Release_TabbedFloating;
    procedure Release_ConjoinedFloating;
    procedure Release_SingleDocked;
    procedure Release_RandomFloating;

    { Test freeing of docked forms when they are all hidden, we don't call
      ProcessMessages between Free calls, but after all Free calls procedure Wait
      is called that calls processMessages multiple times
    }
    procedure FreeHidden_TabbedFloating;
    procedure FreeHidden_ConjoinedFloating;
    procedure FreeHidden_SingleDocked;
    procedure FreeHidden_RandomFloating;

    { Test showing and hiding of docked forms. When calling ShowDockForm we
      want that the dockform becomes visible and the TJvDockClient fires 1
      OnFormShow event; When calling HideDockForm we want that the dockform
      becomes hidden and the TJvDockClient fires 1 OnFormHide event.

      Note that we don't look at the nr of OnShow, OnHide events fired by the
      dock form itself. (because then all test would fail :) )
    }
    procedure ShowHideDockForm_SingleFloating;
    procedure ShowHideDockForm_SingleDocked;
    procedure ShowHideDockForm_TabbedFloating;
    procedure ShowHideDockForm_TabbedDocked;

    { Mantis 0002771: Error after close JvDockableForm with children’s forms }
    procedure ShowHideDockForm_ConjoinedFloating;
    procedure ShowHideDockForm_ConjoinedDocked;
    procedure ShowHideDockForm_RandomFloating;
  end;

implementation

uses
  SysUtils, Dialogs;

type
  TJvDockCustomControlAccess = class(TJvDockCustomControl);

  TWindowList = class(TObjectList)
  private
    FNoWait: Boolean;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure ReleaseAll;
    property NoWait: Boolean read FNoWait write FNoWait;
  end;

const
  cMinimalWait = 0.05;

  cRandomMinWindowCount = 3;
  cRandomMaxWindowCount = 7;

var
  GServerForm: TServerForm;

//=== Local procedures =======================================================

{ Simple wait routine }

procedure Wait(const SecCount: Double);
const
  c1Sec = 1 / (24 * 60 * 60);
var
  EndTime: TDateTime;
begin
  EndTime := Now + c1Sec * SecCount;
  while Now < EndTime do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

function CreateUniqueName: string;
var
  I: Integer;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Screen.FormCount - 1 do
      if CompareText(AName, Screen.Forms[I].Name) = 0 then
        Exit;
    Result := True;
  end;

begin
  for I := 1 to MaxInt do
  begin
    Result := Format('ClientForm_%d', [I]);
    if IsUnique(Result) then
      Exit;
  end;
end;

function IsParentOf(Parent, Client: TWinControl): Boolean;
begin
  while (Parent <> Client) and Assigned(Client) do
    Client := Client.Parent;
  Result := Assigned(Client);
end;

function HasSameParent(Frm1, Frm2: TWinControl): Boolean;
begin
  while Frm1.Parent <> nil do
    Frm1 := Frm1.Parent;
  while Frm2.Parent <> nil do
    Frm2 := Frm2.Parent;
  Result := Frm1 = Frm2;
end;

//=== { TClientForm } ========================================================

constructor TClientForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  Width := 300;
  Height := 250;
  BorderStyle := bsSizeToolWin;
  DockSite := True;
  DragKind := dkDock;
  DragMode := dmAutomatic;
  FormStyle := fsStayOnTop;
  Position := poDefaultPosOnly;

  FDockClient := TJvDockClient.Create(Self);
  with FDockClient do
  begin
    OnFormShow := DockClient1FormShow;
    OnFormHide := DockClient1FormHide;
    DirectDrag := True;
    ShowHint := True;
    EnableCloseButton := True;
    EachOtherDock := False;
  end;

  OnShow := HandleShow;
  OnHide := HandleHide;
end;

procedure TClientForm.DockClient1FormHide(Sender: TObject);
begin
  Inc(FOnFormHideCount);
end;

procedure TClientForm.DockClient1FormShow(Sender: TObject);
begin
  Inc(FOnFormShowCount);
end;

procedure TClientForm.HandleHide(Sender: TObject);
begin
  Inc(FOnHideCount);
end;

procedure TClientForm.HandleShow(Sender: TObject);
begin
  Inc(FOnShowCount);
end;

procedure TClientForm.ResetCounters;
begin
  FOnFormShowCount := 0;
  FOnFormHideCount := 0;
  FOnShowCount := 0;
  FOnHideCount := 0;
end;

//=== { TJvDockingTestCase } =================================================

procedure TJvDockingTestCase.CheckFormCount;
begin
  CheckEquals(FFormCount, Screen.CustomFormCount, 'CustomFormCount');
end;

procedure TJvDockingTestCase.CheckFormVisible(const ExpValue: Boolean;
  AForm: TCustomForm);
begin
  CheckEquals(ExpValue, AForm.Visible, 'Visible');
end;

procedure TJvDockingTestCase.CheckHideDockForm(AClientForm: TClientForm);
begin
  // form must be visible
  CheckFormVisible(True, AClientForm);
  AClientForm.ResetCounters;
  // do the actual call
  HideDockForm(AClientForm);
  //  CheckEquals(1, AClientForm.OnHideCount, 'OnHide');
  CheckEquals(1, AClientForm.OnFormHideCount, 'OnFormHide');
  //  CheckEquals(0, AClientForm.OnShowCount, 'OnShow');
  CheckEquals(0, AClientForm.OnFormShowCount, 'OnFormShow');
  // form must now be hidden
  CheckFormVisible(False, AClientForm);
end;

procedure TJvDockingTestCase.CheckShowDockForm(AClientForm: TClientForm);
begin
  // form must be hidden
  CheckFormVisible(False, AClientForm);
  AClientForm.ResetCounters;
  // do the actual call
  ShowDockForm(AClientForm);
  //  CheckEquals(0, AClientForm.OnHideCount, 'OnHide');
  CheckEquals(0, AClientForm.OnFormHideCount, 'OnFormHide');
  //  CheckEquals(1, AClientForm.OnShowCount, 'OnShow');
  CheckEquals(1, AClientForm.OnFormShowCount, 'OnFormShow');
  // form must now be visible
  CheckFormVisible(True, AClientForm);
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_2ClientForms(AClientForm1,
  AClientForm2: TClientForm);
begin
  CheckHideDockForm(AClientForm1);
  CheckHideDockForm(AClientForm2);
  CheckShowDockForm(AClientForm1);
  CheckShowDockForm(AClientForm2);

  CheckHideDockForm(AClientForm2);
  CheckHideDockForm(AClientForm1);
  CheckShowDockForm(AClientForm2);
  CheckShowDockForm(AClientForm1);

  CheckHideDockForm(AClientForm2);
  CheckShowDockForm(AClientForm2);

  CheckHideDockForm(AClientForm1);
  CheckShowDockForm(AClientForm1);

  CheckHideDockForm(AClientForm1);
  CheckHideDockForm(AClientForm2);

  CheckShowDockForm(AClientForm2);
  CheckHideDockForm(AClientForm2);

  CheckShowDockForm(AClientForm1);
  CheckHideDockForm(AClientForm1);
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_ClientList(
  AList: TList);
begin
  CheckShowHideDockForm_ShowAll(AList);
  CheckShowHideDockForm_HideAll(AList);
  CheckShowHideDockForm_ShowOne(AList);
  CheckShowHideDockForm_HideOne(AList);
  CheckShowHideDockForm_ShowTwo(AList);
  CheckShowHideDockForm_HideTwo(AList);
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_HideAll(AList: TList);
var
  I: Integer;
  Frm: TClientForm;
begin
  { Hide all forms in AList }
  for I := 0 to AList.Count - 1 do
    if TObject(AList[i]) is TClientForm then
    begin
      Frm := TClientForm(AList[i]);
      if Frm.Visible then
        CheckHideDockForm(Frm);
    end;
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_HideOne(AList: TList);
var
  I: Integer;
  Frm: TClientForm;
begin
  { Show all forms in AList, then hide them one-by-one }
  CheckShowHideDockForm_ShowAll(AList);

  for I := 0 to AList.Count - 1 do
    if TObject(AList[i]) is TClientForm then
    begin
      Frm := TClientForm(AList[i]);

      CheckHideDockForm(Frm);
      CheckShowDockForm(Frm);
    end;
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_HideTwo(AList: TList);
var
  I, J: Integer;
  Frm1, Frm2: TClientForm;
begin
  { Show all forms in AList, then hide them in pairs }
  CheckShowHideDockForm_ShowAll(AList);

  for I := 0 to AList.Count - 2 do
    if TObject(AList[i]) is TClientForm then
    begin
      Frm1 := TClientForm(AList[i]);

      for J := I + 1 to AList.Count - 1 do
        if TObject(AList[J]) is TClientForm then
        begin
          Frm2 := TClientForm(AList[J]);

          CheckHideDockForm(Frm1);
          CheckHideDockForm(Frm2);

          CheckShowDockForm(Frm1);
          CheckShowDockForm(Frm2);

          CheckHideDockForm(Frm1);
          CheckHideDockForm(Frm2);

          CheckShowDockForm(Frm2);
          CheckShowDockForm(Frm1);

          CheckHideDockForm(Frm2);
          CheckHideDockForm(Frm1);

          CheckShowDockForm(Frm1);
          CheckShowDockForm(Frm2);

          CheckHideDockForm(Frm2);
          CheckHideDockForm(Frm1);

          CheckShowDockForm(Frm2);
          CheckShowDockForm(Frm1);
        end;
    end;
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_ShowAll(AList: TList);
var
  I: Integer;
  Frm: TClientForm;
begin
  { Show all forms in AList }
  for I := 0 to AList.Count - 1 do
    if TObject(AList[i]) is TClientForm then
    begin
      Frm := TClientForm(AList[i]);
      if not Frm.Visible then
        CheckShowDockForm(Frm);
    end;
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_ShowOne(AList: TList);
var
  I: Integer;
  Frm: TClientForm;
begin
  { Hide all forms in AList, then show them in one-by-one }
  CheckShowHideDockForm_HideAll(AList);

  for I := 0 to AList.Count - 1 do
    if TObject(AList[i]) is TClientForm then
    begin
      Frm := TClientForm(AList[i]);

      CheckShowDockForm(Frm);
      CheckHideDockForm(Frm);
    end;
end;

procedure TJvDockingTestCase.CheckShowHideDockForm_ShowTwo(AList: TList);
var
  I, J: Integer;
  Frm1, Frm2: TClientForm;
begin
  { Hide all forms in AList, then show them in pairs }
  CheckShowHideDockForm_HideAll(AList);

  for I := 0 to AList.Count - 2 do
    if TObject(AList[i]) is TClientForm then
    begin
      Frm1 := TClientForm(AList[i]);

      for J := I + 1 to AList.Count - 1 do
        if TObject(AList[J]) is TClientForm then
        begin
          Frm2 := TClientForm(AList[J]);

          CheckShowDockForm(Frm1);
          CheckShowDockForm(Frm2);

          CheckHideDockForm(Frm1);
          CheckHideDockForm(Frm2);

          CheckShowDockForm(Frm1);
          CheckShowDockForm(Frm2);

          CheckHideDockForm(Frm2);
          CheckHideDockForm(Frm1);

          CheckShowDockForm(Frm2);
          CheckShowDockForm(Frm1);

          CheckHideDockForm(Frm1);
          CheckHideDockForm(Frm2);

          CheckShowDockForm(Frm2);
          CheckShowDockForm(Frm1);

          CheckHideDockForm(Frm2);
          CheckHideDockForm(Frm1);
        end;
    end;
end;

procedure TJvDockingTestCase.ClearLog;
begin
  FLog.Clear;
end;

function TJvDockingTestCase.ConstructClientForm: TClientForm;
begin
  Result := TClientForm.Create(Application);
  Result.Visible := True;
  Result.Name := CreateUniqueName;
  Result.DockClient.DockStyle := ServerForm.DefaultStyle;
  Result.DockClient.DirectDrag := False;
  Result.DockClient.EachOtherDock := True;
end;

procedure TJvDockingTestCase.ConstructClientList(AList: TList;
  const ACount: Integer);
var
  SingleClientForms, FloatingForms: TList;
  F1, F2: TForm;
  F: TForm;
  I1, I2: Integer;

  procedure RecalculateFloatingForms;
  var
    Frm: TForm;
    I: Integer;
  begin
    FloatingForms.Clear;
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[i] is TClientForm then
      begin
        Frm := Screen.Forms[i];
        Frm := TForm(GetParentForm(Frm));
        if FloatingForms.IndexOf(Frm) < 0 then
          FloatingForms.Add(Frm);
      end;
  end;
begin
  ClearLog;

  AList.Clear;
  SingleClientForms := TList.Create;
  FloatingForms := TList.Create;
  try
    while AList.Count < ACount do
    begin
      AList.Add(ConstructClientForm);
    end;
    SingleClientForms.Assign(AList);
    FloatingForms.Assign(AList);
    while FloatingForms.Count > 1 do
    begin
      case Trunc(Random(3)) of
        0:
          if SingleClientForms.Count > 1 then
          begin
            // conjoin dock
            repeat
              I1 := Trunc(Random(SingleClientForms.Count));
              I2 := Trunc(Random(SingleClientForms.Count));
            until I1 <> I2;

            F1 := TClientForm(SingleClientForms[i1]);
            F2 := TClientForm(SingleClientForms[i2]);

            LogFmt('Conjoin dock %s and %s', [F1.Name, F2.Name]);

            F := ManualConjoinDock(nil, F1, F2);
            SingleClientForms.Remove(F1);
            SingleClientForms.Remove(F2);
            FloatingForms.Remove(F1);
            FloatingForms.Remove(F2);
            FloatingForms.Add(F);

            Wait(cMinimalWait);
          end;
        1:
          if SingleClientForms.Count > 1 then
          begin
            // tab dock
            repeat
              I1 := Trunc(Random(SingleClientForms.Count));
              I2 := Trunc(Random(SingleClientForms.Count));
            until I1 <> I2;

            F1 := TClientForm(SingleClientForms[i1]);
            F2 := TClientForm(SingleClientForms[i2]);

            LogFmt('Tab dock %s and %s', [F1.Name, F2.Name]);

            F := ManualTabDock(nil, F1, F2);
            SingleClientForms.Remove(F1);
            SingleClientForms.Remove(F2);
            FloatingForms.Remove(F1);
            FloatingForms.Remove(F2);
            FloatingForms.Add(F);

            Wait(cMinimalWait);
          end;
      else
        if (AList.Count > 2) and (FloatingForms.Count > 0) then
        begin
          repeat
            I1 := Trunc(Random(FloatingForms.Count));
            F1 := TForm(FloatingForms[i1]);
            I2 := Trunc(Random(AList.Count));
            F2 := TForm(AList[I2]);
          until (F1 <> F2) and
            not IsParentOf(F1, F2) and not IsParentOf(F2, F1) and
            not HasSameParent(F1, F2);

          if F2 is TClientForm then
          begin
            if Assigned(F2.HostDockSite) then
            begin
              LogFmt('Drop %s onto %s', [F1.Name, F2.Name]);

              DoDock(F2.HostDockSite, F1);
              //            F1.ManualDock(F2, nil, alLeft);
              SingleClientForms.Remove(F1);
              SingleClientForms.Remove(F2);
              FloatingForms.Remove(F1);

              // form F2 can be deleted
              RecalculateFloatingForms;
              Wait(cMinimalWait);
            end;
          end;
        end;
      end;
    end;
  finally
    SingleClientForms.Free;
    FloatingForms.Free;
  end;
end;

procedure TJvDockingTestCase.DoDock(Target, DropControl: TWinControl);
var
  ADockClient: TJvDockClient;
  DragObject: TJvDockDragDockObject;
  X, Y: Integer;
begin
  X := 0;
  Y := 0;

  DragObject := TJvDockDragDockObject.Create(DropControl);
  try
    DragObject.DropOnControl := DropControl;
    with DropControl.BoundsRect do
      DragObject.DockRect := Rect(Left, Top, (Left + Right) div 2, Bottom);
    DragObject.DropAlign := alLeft;
    if Target is TJvDockCustomControl then
      TJvDockCustomControlAccess(Target).CustomDockDrop(DragObject, X, Y)
    else
      if Target is TForm then
    begin
      ADockClient := FindDockClient(Target);
      if ADockClient <> nil then
        ADockClient.FormDockDrop(DragObject, X, Y);
    end;
  finally
    DragObject.Free;
  end;
  Wait(cMinimalWait);
end;

procedure TJvDockingTestCase.FreeHidden_ConjoinedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  SaveFormCount;

  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualConjoinDock(nil, ClientForm1, ClientForm2);
    Wait(cMinimalWait);
    CheckHideDockForm(ClientForm1);
    CheckHideDockForm(ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.FreeHidden_RandomFloating;
var
  ClientList: TWindowList;
  I: Integer;
  Count: Integer;
begin
  for Count := cRandomMinWindowCount to cRandomMaxWindowCount do
    for I := 0 to Count do
    begin
      SaveFormCount;

      try
        ClientList := TWindowList.Create;
        try
          ConstructClientList(ClientList, Count);
          Wait(cMinimalWait);

          CheckShowHideDockForm_HideAll(ClientList);
        finally
          ClientList.Free;
        end;

        Wait(cMinimalWait);
        CheckFormCount;
      except
        ShowLog;
        raise;
      end;
    end;
end;

procedure TJvDockingTestCase.FreeHidden_SingleDocked;
var
  ClientForm: TClientForm;
begin
  SaveFormCount;

  ClientForm := ConstructClientForm;
  try
    ClientForm.ManualDock(ServerForm.DockServer.LeftDockPanel);

    CheckHideDockForm(ClientForm);

    Wait(cMinimalWait);
  finally
    ClientForm.Free;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.FreeHidden_TabbedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  SaveFormCount;

  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualTabDock(nil, ClientForm1, ClientForm2);
    Wait(cMinimalWait);
    CheckHideDockForm(ClientForm1);
    CheckHideDockForm(ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.Free_ConjoinedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  SaveFormCount;

  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualConjoinDock(nil, ClientForm1, ClientForm2);

    Wait(cMinimalWait);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.Free_RandomFloating;
var
  ClientList: TWindowList;
  I: Integer;
  Count: Integer;
begin
  for Count := cRandomMinWindowCount to cRandomMaxWindowCount do
    for I := 0 to Count do
    begin
      SaveFormCount;

      try
        ClientList := TWindowList.Create;
        try
          ClientList.NoWait := True;
          ConstructClientList(ClientList, Count);
          Wait(cMinimalWait);
        finally
          ClientList.Free;
        end;

        Wait(cMinimalWait);
        CheckFormCount;
      except
        ShowLog;
        raise;
      end;
    end;
end;

procedure TJvDockingTestCase.Free_SingleDocked;
var
  ClientForm: TClientForm;
begin
  SaveFormCount;

  ClientForm := ConstructClientForm;
  try
    ClientForm.ManualDock(ServerForm.DockServer.LeftDockPanel);

    Wait(cMinimalWait);
  finally
    ClientForm.Free;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.Free_TabbedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  SaveFormCount;

  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualTabDock(nil, ClientForm1, ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

function TJvDockingTestCase.GetServerForm: TServerForm;
begin
  if GServerForm = nil then
  begin
    GServerForm := TServerForm.Create(Application);
    GServerForm.Visible := True;
    GServerForm.Name := 'ServerForm';
    GServerForm.OnDestroy := HandleServerFormDestroy;
  end;

  Result := GServerForm;
end;

procedure TJvDockingTestCase.HandleServerFormDestroy(Sender: TObject);
begin
  GServerForm := nil;
end;

procedure TJvDockingTestCase.Log(const Msg: string);
begin
  FLog.Add(Msg);
end;

procedure TJvDockingTestCase.LogFmt(const Format: string;
  const Args: array of const);
begin
  Log(SysUtils.Format(Format, Args));
end;

procedure TJvDockingTestCase.Release_ConjoinedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  SaveFormCount;

  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualConjoinDock(nil, ClientForm1, ClientForm2);
  finally
    ClientForm1.Release;
    ClientForm2.Release;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.Release_RandomFloating;
var
  ClientList: TWindowList;
  I: Integer;
  Count: Integer;
begin
  for Count := cRandomMinWindowCount to cRandomMaxWindowCount do
    for I := 0 to Count do
    begin
      SaveFormCount;

      try
        ClientList := TWindowList.Create;
        try
          ConstructClientList(ClientList, Count);
          Wait(cMinimalWait);

          CheckShowHideDockForm_HideAll(ClientList);
        finally
          ClientList.ReleaseAll;
          ClientList.Free;
        end;

        Wait(cMinimalWait);
        CheckFormCount;
      except
        ShowLog;
        raise;
      end;
    end;
end;

procedure TJvDockingTestCase.Release_SingleDocked;
var
  ClientForm: TClientForm;
begin
  SaveFormCount;

  ClientForm := ConstructClientForm;
  try
    ClientForm.ManualDock(ServerForm.DockServer.LeftDockPanel);
  finally
    ClientForm.Release;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.Release_TabbedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  SaveFormCount;

  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualTabDock(nil, ClientForm1, ClientForm2);
  finally
    ClientForm1.Release;
    ClientForm2.Release;
  end;

  Wait(cMinimalWait);
  CheckFormCount;
end;

procedure TJvDockingTestCase.SaveFormCount;
begin
  FFormCount := Screen.CustomFormCount;
end;

procedure TJvDockingTestCase.SetUp;
begin
  FLog := TStringList.Create;

  // construct server form:
  ServerForm;
end;

procedure TJvDockingTestCase.ShowHideDockForm_ConjoinedDocked;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualConjoinDock(ServerForm.DockServer.LeftDockPanel, ClientForm1, ClientForm2);
    Wait(cMinimalWait);

    CheckShowHideDockForm_2ClientForms(ClientForm1, ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;
end;

procedure TJvDockingTestCase.ShowHideDockForm_ConjoinedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualConjoinDock(nil, ClientForm1, ClientForm2);
    Wait(cMinimalWait);

    CheckShowHideDockForm_2ClientForms(ClientForm1, ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;
end;

procedure TJvDockingTestCase.ShowHideDockForm_RandomFloating;
var
  ClientList: TWindowList;
  I: Integer;
  Count: Integer;
begin
  for Count := cRandomMinWindowCount to cRandomMaxWindowCount do
    for I := 0 to Count do
    begin
      SaveFormCount;

      try
        ClientList := TWindowList.Create;
        try
          ConstructClientList(ClientList, Count);
          Wait(cMinimalWait);

          CheckShowHideDockForm_ClientList(ClientList);
        finally
          ClientList.Free;
        end;

        Wait(cMinimalWait);
        CheckFormCount;
      except
        ShowLog;
        raise;
      end;
    end;
end;

procedure TJvDockingTestCase.ShowHideDockForm_SingleDocked;
var
  ClientForm: TClientForm;
begin
  ClientForm := ConstructClientForm;
  try
    ClientForm.ManualDock(ServerForm.DockServer.LeftDockPanel);
    CheckHideDockForm(ClientForm);
    CheckShowDockForm(ClientForm);
    CheckHideDockForm(ClientForm);
    CheckShowDockForm(ClientForm);
  finally
    ClientForm.Free;
  end;
end;

procedure TJvDockingTestCase.ShowHideDockForm_SingleFloating;
var
  ClientForm: TClientForm;
begin
  ClientForm := ConstructClientForm;
  try
    CheckHideDockForm(ClientForm);
    CheckShowDockForm(ClientForm);
    CheckHideDockForm(ClientForm);
    CheckShowDockForm(ClientForm);
  finally
    ClientForm.Free;
  end;
end;

procedure TJvDockingTestCase.ShowHideDockForm_TabbedDocked;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualTabDock(ServerForm.DockServer.LeftDockPanel, ClientForm1, ClientForm2);

    CheckShowHideDockForm_2ClientForms(ClientForm1, ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;
end;

procedure TJvDockingTestCase.ShowHideDockForm_TabbedFloating;
var
  ClientForm1, ClientForm2: TClientForm;
begin
  ClientForm1 := ConstructClientForm;
  ClientForm2 := ConstructClientForm;
  try
    ManualTabDock(nil, ClientForm1, ClientForm2);
    Wait(cMinimalWait);

    CheckShowHideDockForm_2ClientForms(ClientForm1, ClientForm2);
  finally
    ClientForm1.Free;
    ClientForm2.Free;
  end;
end;

procedure TJvDockingTestCase.ShowLog;
begin
  ShowMessage(FLog.Text);
end;

procedure TJvDockingTestCase.TearDown;
begin
  FLog.Free;
end;

//=== { TServerForm } ========================================================

constructor TServerForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  Width := 186;
  Height := 188;
  DockSite := True;
  DragKind := dkDock;
  DragMode := dmAutomatic;
  Position := poDefaultPosOnly;

  FDockServer := TJvDockServer.Create(Self);
  with FDockServer do
  begin
    DockStyle := DefaultStyle;
  end;
end;

function TServerForm.GetDefaultStyle: TJvDockBasicStyle;
begin
  Result := DockVSNetStyle;
//  Result := DockVIDStyle;
end;

function TServerForm.GetDockVIDStyle: TJvDockVIDStyle;
begin
  if FDockVIDStyle = nil then
    FDockVIDStyle := TJvDockVIDStyle.Create(Self);
  Result := FDockVIDStyle;
end;

function TServerForm.GetDockVSNetStyle: TJvDockVSNetStyle;
begin
  if FDockVSNetStyle = nil then
    FDockVSNetStyle := TJvDockVSNetStyle.Create(Self);
  Result := FDockVSNetStyle;
end;

//=== { TWindowList } ========================================================

procedure TWindowList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if (Action = lnDeleted) and not NoWait then
    Wait(cMinimalWait);
end;

procedure TWindowList.ReleaseAll;
var
  SavedOwnsObjects: Boolean;
  I: Integer;
begin
  SavedOwnsObjects := OwnsObjects;
  try
    OwnsObjects := False;
    I := 0;
    while I < Count do
    begin
      if Items[i] is TForm then
      begin
        TForm(Items[i]).Release;
        Delete(I);
      end
      else
        Inc(I);
    end;
  finally
    OwnsObjects := SavedOwnsObjects;
  end;
end;

initialization
  TestFrameWork.RegisterTest('Docking', TJvDockingTestCase.Suite);
end.
