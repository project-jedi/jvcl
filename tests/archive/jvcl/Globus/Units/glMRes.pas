unit glMRes;

interface
{$I glDEF.INC}
uses Windows, Controls, Classes, Forms, SysUtils, Dialogs, DsgnIntf, TypInfo;
type

  TResStringList = class(TStringList)
  end;

  TglMultiResources = class(TComponent)
  private
    FComps: TStringList;
    FResources: TResStringList;
//    FCompList: TList;
    function GetName( Component: TComponent ): string;
    procedure ProcessNewComponent( Component: TComponent );
    procedure GetComponentData(C: TComponent; SLNames: TStringList);
    procedure GetPropEditor(Prop: TPropertyEditor);
    procedure E(const S: string);
  protected
    procedure Notification( Component: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;
  public
//    property CompList: TList read FCompList write FCompList;
    procedure Update;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Resources: TResStringList read FResources write FResources;
    property Comps: TStringList read FComps write FComps;
end;

procedure Register;

implementation
uses StdCtrls;//, mresed;

procedure Register;
begin
  RegisterComponents('Proba', [TglMultiResources]);
//  RegisterPropertyEditor( TypeInfo(TResStringList), TglMultiResources, 'Resources', TglResourcesProperty );
end;

constructor TglMultiResources.Create(AOwner: TComponent);
var i, j: integer;
begin
  inherited;
  FResources := TResStringList.Create;
  FComps := TStringList.Create;
//  FCompList := TList.Create;
{  FResources.Add('Form1#Label1#Caption1');
  FResources.Add('Form2#Label2#Caption2');
  FResources.Add('Form3#Label3#Caption3');
  FResources.Add('Form4#Label4#Caption4');}
  if (csDesigning in ComponentState)and(FComps.Count=0) then
  with TForm(AOwner) do
  begin
    for i:=0 to ControlCount-1 do
      if TComponent(Comps[i]) is TLabel then FComps.Add( TComponent(Comps[i]).Name );
        //FResources.Add( Comps[i].Name+'#'+TLabel(Comps[i]).Caption );
  end;
end;

destructor TglMultiResources.Destroy;
begin
 FComps.Free;
 FResources.Free;
// FCompList.Free;
 inherited Destroy;
end;

procedure TglMultiResources.Notification( Component: TComponent; Operation: TOperation );
var i, j: integer;
begin
  inherited;
//  if (Component <> Self)and(Operation = opInsert) then ProcessNewComponent(Component);
  //if (Component <> Self)and(Operation = opRename)and(Assigned(LastComponent)) then ProcessNewComponent(Component);
//    raise Exception.Create('Cannot create more than one instance of TglMultiResources component');
  if (Component <> Self)and(Operation = opInsert) then
    if (Component is TLabel)or(Component is TListBox) then
    begin
      Comps.AddObject(Component.name, Component);
      Resources.Add('');
    end;
{  if (Component <> Self)and(Operation = opRemove) then
    for i := 0 to CompList.Count-1 do if CompList[i] = Component then
    begin
      CompList.Delete(i);
      //for j := Comps.Count-1 downto 0 do
      //  if pos( Component.Name Comps[] )
      break;
    end;}
end;

procedure TglMultiResources.Update;
var
  i, j, Ind: integer;
  c: TComponent;
  cName: string;
  SLNames: TStringList;
begin

  SLNames := TStringList.Create;
  try
    for i:=0 to Comps.Count-1 do
    begin
      cName := '';
      C := TComponent(Comps.Objects[i]);
      if C = nil then begin
        for j:=1 to length(Comps[i]) do if Comps[i][j]='.' then break;
        cName := copy(Comps[i], 0, j-1);
        C := Owner.FindComponent(cName);
      end;
      if C = nil then exit;

      if cName<>'' then for j:=0 to Comps.Count-1 do
        if pos( cName+'.', Comps[i] ) = 1 then Comps.Objects[i] := C;

      if C.Name = '' then continue;
      SLNames.Clear; //SLNames.Sorted := true;
      GetComponentData(C, SLNames);
      for j := 0 to SLNames.Count-1 do
      begin
        //IndexOfObject(
        Ind := Comps.IndexOf( C.Name + '.' + SLNames.Names[j] );
        if Ind = -1 then
        begin

          Comps.Insert( Comps.IndexOfObject(C), C.Name + '.' + SLNames.Names[j] );
          Resources.Add( SLNames.Values[ SLNames.Names[j] ] );
        end else
          Resources[Ind] := SLNames.Values[ SLNames.Names[j] ];
      end;
      if Comps[Comps.IndexOfObject(C)] = '' then
      begin
        Resources.Delete(Comps.IndexOfObject(C)-1);
        Comps.Delete(Comps.IndexOfObject(C));
      end;
    end;
  finally
    SLNames.Free;
  end;

end;

procedure TglMultiResources.GetComponentData(C: TComponent; SLNames: TStringList);
var i: integer;
begin
  if C is TLabel then
  begin
    SLNames.Add( 'Caption='+TLabel(C).Caption );
  end else
  if C is TListBox then
    for i:=0 to TListBox(C).Items.Count-1 do
    begin
      SLNames.Add( 'Items['+IntToStr(i)+']=' + TListBox(C).Items[i] );
    end;
end;

procedure TglMultiResources.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
  end;
end;

function TglMultiResources.GetName( Component: TComponent ): string;
var
  i: integer;
  BaseName: string;
begin
  if Component.Name <> '' then begin Result := Component.Name; exit; end;
  i := 1;
  BaseName := copy( Component.ClassName, 2, length(Component.ClassName));
  while Owner.FindComponent(BaseName+IntToStr(i)) <> nil do inc(i);
  Result := BaseName+IntToStr(i);
end;

procedure TglMultiResources.ProcessNewComponent( Component: TComponent );
var
  SLNames, SLValues: TStringList;
  FormDesigner: TFormDesigner;
  CompList: TComponentList;
  i: integer;
begin
  if not(Component is TControl) then exit;
  SLNames := TStringList.Create;
  SLValues := TStringList.Create;
//  CompList := TComponentList.Create;
//  FormDesigner := TFormDesigner.Create;
  if (Component is TLabel)or(Component is TListBox) then CompList.Add(Component);

  try
{    if Component is TLabel then
      Comps.Add( GetName( Component ) );
    if Component is TListBox then //with TListBox(Component) do
      for i:=0 to TListBox(Component).Items.Count-1 do
        Comps.Add( GetName( Component )+'.Items['+IntToStr(i)+']' );
}
  finally
///    FormDesigner.Free;
//    CompList.Free;
    SLNames.Free;
    SLValues.Free;
  end;
end;

procedure TglMultiResources.E(const S: string);
begin
//  ShowMessage(s);
end;

procedure TglMultiResources.GetPropEditor(Prop: TPropertyEditor);
begin
//  ShowMessage(s);
end;

end.
