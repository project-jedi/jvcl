{******************************************************************************}
{*                                                                            *}
{* PasDesigner 0.1 - Package loading                                          *}
{*                                                                            *}
{* (C) 2003-2004 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit PDPackageLoader;

{$I jedi.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Graphics, ImgList, Controls, ActnList,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, QGraphics, QImgList, QActnList,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, TypInfo;

const
  sNoIconPalette = '.';

type
{ Interfaces }

  IPackage = interface;
  IPackageLoader = interface;
  IUnit = interface;
  IComponentItem = interface;
  IActionItem = interface;

  IPackage = interface
    ['{7BA3BF7B-EA58-46B1-8E8B-0159F567287C}']
  {private}
    function GetIsRunOnly: Boolean;
    function GetFlags: Integer;
    function GetName: string;
    function GetDescription: string;
    function GetDcpBpiName: string;
    function GetHandle: THandle;

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): string;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;
  {public}
    property IsRunOnly: Boolean read GetIsRunOnly;
    property Flags: Integer read GetFlags;

    property Name: string read GetName;
    property Description: string read GetDescription;
    property DcpBpiName: string read GetDcpBpiName;
    property Handle: THandle read GetHandle;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: string read GetRequires;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;
  end;

  IPackageLoader = Interface
    ['{E69EEA58-87A9-4D23-B716-7735F7B0D7B9}']
  {private}
    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetPackageCount: Integer;
    function GetPackages(Index: Integer): IPackage;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;
  {public}
    function AddPackage(const FileName: string): Boolean; overload;
    function AddPackage(Handle: THandle): Boolean; overload;

    procedure RemovePackage(const Name: string); overload;
    procedure RemovePackage(Index: Integer); overload;

    function IndexOf(const Name: string): Integer; overload;
    function IndexOf(Handle: THandle): Integer; overload;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateImageList;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property PackageCount: Integer read GetPackageCount;
    property Packages[Index: Integer]: IPackage read GetPackages;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;
  end;

  IUnit = interface
    ['{C3D68986-4D02-48E9-8395-B830DB9D533F}']
  {private}
    function GetName: string;
    function GetFlags: Integer;
  {public}
    property Name: string read GetName;
    property Flags: Integer read GetFlags;
  end;

  IComponentItem = interface
    ['{9086C9A0-1149-48A1-8AD5-6009A104DFFD}']
  {private}
    function GetComponentClass: TComponentClass;
    function GetPalette: string;
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetUnitName: string;
  {public}
    property Palette: string read GetPalette;
    property ComponentClass: TComponentClass read GetComponentClass;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property UnitName: string read GetUnitName;
  end;

  IActionItem = interface
    ['{985AF26E-C584-4C9F-AADD-82DF0958CF8E}']
  {private}
    function GetActionClass: TBasicActionClass;
    function GetCategory: string;
    function GetResource: TComponentClass;
    function GetUnitName: string;
  {public}
    property Category: string read GetCategory;
    property ActionClass: TBasicActionClass read GetActionClass;
    property Resource: TComponentClass read GetResource;
    property UnitName: string read GetUnitName;
  end;


{ Classes }

  TPackage = class;
  TPackageLoader = class;
  TUnit = class;
  TComponentItem = class;

  TPackage = class(TInterfacedObject, IPackage)
  private
    FPackageLoader: TPackageLoader;
    FHandle: THandle;
    FUnits: TInterfaceList;
    FComponents: TInterfaceList;
    FActions: TInterfaceList;
    FFlags: Integer;
    FRequires: TStrings;
    FName: string;
    FDescription: string;
    FDcpBpiName: string;

    function GetIsRunOnly: Boolean;
    function GetFlags: Integer;
    function GetName: string;
    function GetDescription: string;
    function GetDcpBpiName: string;
    function GetHandle: THandle;

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;

    function GetRequireCount: Integer;
    function GetRequires(Index: Integer): string;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;
  public
    constructor Create(AHandle: THandle; APackageLoader: TPackageLoader);
    destructor Destroy; override;

    property IsRunOnly: Boolean read GetIsRunOnly;
    property Flags: Integer read GetFlags;

    property Name: string read GetName;
    property Description: string read GetDescription;
    property DcpBpiName: string read GetDcpBpiName;
    property Handle: THandle read GetHandle;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property RequireCount: Integer read GetRequireCount;
    property Requires[Index: Integer]: string read GetRequires;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;
  end;

  TPackageLoader = class(TComponent, IPackageLoader)
  private
    FPackages: TInterfaceList;
    FComponents: TInterfaceList;
    FActions: TInterfaceList;
    FLock: TRTLCriticalSection;
    FCurrentPackage: TPackage;
    FImageList: TImageList;
    FLockCount: Integer;

    function GetUnitCount: Integer;
    function GetUnits(Index: Integer): IUnit;
    
    function GetPackageCount: Integer;
    function GetPackages(Index: Integer): IPackage;

    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): IComponentItem;

    function GetActionCount: Integer;
    function GetActions(Index: Integer): IActionItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddPackage(const FileName: string): Boolean; overload;
    function AddPackage(Handle: THandle): Boolean; overload;

    procedure RemovePackage(const Name: string); overload;
    procedure RemovePackage(Index: Integer); overload;

    function IndexOf(const Name: string): Integer; overload;
    function IndexOf(Handle: THandle): Integer; overload;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateImageList;

    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IUnit read GetUnits;

    property PackageCount: Integer read GetPackageCount;
    property Packages[Index: Integer]: IPackage read GetPackages;

    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: IComponentItem read GetComponents;

    property ActionCount: Integer read GetActionCount;
    property Actions[Index: Integer]: IActionItem read GetActions;

    property ImageList: TImageList read FImageList write FImageList;
  end;

  TUnit = class(TInterfacedObject, IUnit)
  private
    FName: string;
    FFlags: Integer;
    function GetName: string;
    function GetFlags: Integer;
  public
    constructor Create(const AName: string; AFlags: Integer);
    property Name: string read GetName;
    property Flags: Integer read GetFlags;
  end;

  TComponentItem = class(TInterfacedObject, IComponentItem)
  private
    FComponentClass: TComponentClass;
    FPalette: string;
    FImageIndex: Integer;
    function GetComponentClass: TComponentClass;
    function GetPalette: string;
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetUnitName: string;
  public
    constructor Create(const APalette: string; AComponentClass: TComponentClass;
      AImageIndex: Integer);
    property Palette: string read GetPalette;
    property ComponentClass: TComponentClass read GetComponentClass;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property UnitName: string read GetUnitName;
  end;

  TActionItem = class(TInterfacedObject, IActionItem)
  private
    FCategory: string;
    FActionClass: TBasicActionClass;
    FResource: TComponentClass;
    function GetActionClass: TBasicActionClass;
    function GetCategory: string;
    function GetResource: TComponentClass;
    function GetUnitName: string;
  public
    constructor Create(const ACategory: string; AActionClass: TBasicActionClass;
      AResource: TComponentClass);

    property Category: string read GetCategory;
    property ActionClass: TBasicActionClass read GetActionClass;
    property Resource: TComponentClass read GetResource;
    property UnitName: string read GetUnitName;
  end;

var
  PackageLoader: TPackageLoader;

implementation

uses
  PDHelpers;

{$R rcCoreIDE.res}

procedure GlobalRegisterComponentsProc(const Page: string;
  {$IFDEF COMPILER7}const{$ENDIF} ComponentClasses: array of TComponentClass);
var
  i: Integer;
  Item: IComponentItem;
begin
  for i := 0 to High(ComponentClasses) do
  begin
    Item := TComponentItem.Create(Page, ComponentClasses[i], 0);
    PackageLoader.FComponents.Add(Item);
    if PackageLoader.FCurrentPackage <> nil then
      PackageLoader.FCurrentPackage.FComponents.Add(Item);
  end;
end;

procedure GlobalRegisterNoIconProc({$IFDEF COMPILER7}const{$ENDIF} ComponentClasses: array of TComponentClass);
begin
  GlobalRegisterComponentsProc(sNoIconPalette, ComponentClasses);
end;

procedure GlobalRegisterActionsProc(const CategoryName: string;
  const AClasses: array of TBasicActionClass; Resource: TComponentClass);
var
  i: Integer;
  Item: IActionItem;
begin
  for i := 0 to High(AClasses) do
  begin
    Item := TActionItem.Create(CategoryName, AClasses[i], Resource);
    PackageLoader.FActions.Add(Item);
    if PackageLoader.FCurrentPackage <> nil then
      PackageLoader.FCurrentPackage.FActions.Add(Item);
  end;
end;

{ TPackageLoader }

function TPackageLoader.AddPackage(Handle: THandle): Boolean;
begin
  if Handle <> 0 then
  begin
    CSBlock(FLock);
    FCurrentPackage := TPackage.Create(Handle, Self);
    try
      FPackages.Add(FCurrentPackage);
    finally
      FCurrentPackage := nil;
    end;
    if FLockCount = 0 then
      UpdateImageList;
    Result := True;
  end
  else
    Result := False;
end;

function TPackageLoader.AddPackage(const FileName: string): Boolean;
begin
  Result := AddPackage(LoadPackage(FileName));
end;

procedure TPackageLoader.BeginUpdate;
begin
  Inc(FLockCount);
end;

constructor TPackageLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackages := TInterfaceList.Create;
  FComponents := TInterfaceList.Create;
  FActions := TInterfaceList.Create;
  InitializeCriticalSection(FLock);

  RegisterComponentsProc := GlobalRegisterComponentsProc;
  RegisterNoIconProc := GlobalRegisterNoIconProc;
  RegisterActionsProc := GlobalRegisterActionsProc;
end;

destructor TPackageLoader.Destroy;
begin
  FActions.Free;
  FComponents.Free;
  FPackages.Free;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TPackageLoader.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    UpdateImageList;
end;

function TPackageLoader.GetComponentCount: Integer;
begin
  CSBlock(FLock);
  Result := FComponents.Count;
end;

function TPackageLoader.GetComponents(Index: Integer): IComponentItem;
begin
  CSBlock(FLock);
  Result := FComponents[Index] as IComponentItem;
end;

function TPackageLoader.GetActionCount: Integer;
begin
  CSBlock(FLock);
  Result := FActions.Count;
end;

function TPackageLoader.GetActions(Index: Integer): IActionItem;
begin
  CSBlock(FLock);
  Result := FActions[Index] as IActionItem;
end;

function TPackageLoader.GetUnitCount: Integer;
var
  i: Integer;
begin
  CSBlock(FLock);
  Result := 0;
  for i := 0 to PackageCount - 1 do
    Inc(Result, Packages[i].UnitCount);
end;

function TPackageLoader.GetUnits(Index: Integer): IUnit;
var
  i: Integer;
begin
  CSBlock(FLock);
  for i := 0 to PackageCount - 1 do
  begin
    if Index < Packages[i].UnitCount then
    begin
      Result := Packages[i].Units[Index];
      Exit;
    end;
    Dec(Index, Packages[i].UnitCount);
  end;
end;

function TPackageLoader.GetPackageCount: Integer;
begin
  CSBlock(FLock);
  Result := FPackages.Count;
end;

function TPackageLoader.GetPackages(Index: Integer): IPackage;
begin
  CSBlock(FLock);
  Result := FPackages[Index] as IPackage;
end;

function TPackageLoader.IndexOf(const Name: string): Integer;
begin
  CSBlock(FLock);

  for Result := 0 to PackageCount - 1 do
    if CompareText(Packages[Result].Name, Name) = 0 then
      Exit;
  Result := -1;
end;

function TPackageLoader.IndexOf(Handle: THandle): Integer;
begin
  CSBlock(FLock);

  for Result := 0 to PackageCount - 1 do
    if Packages[Result].Handle = Handle then
      Exit;
  Result := -1;
end;

procedure TPackageLoader.RemovePackage(const Name: string);
var Index: Integer;
begin
  CSBlock(FLock);

  Index := IndexOf(Name);
  if Index <> -1 then
  begin
    FPackages.Delete(Index);
    if FLockCount = 0 then
      UpdateImageList;
  end;
end;

procedure TPackageLoader.RemovePackage(Index: Integer);
begin
  CSBlock(FLock);

  if (Index >= 0) and (Index < PackageCount) then
  begin
    FPackages.Delete(Index);
    if FLockCount = 0 then
      UpdateImageList;
  end;
end;

procedure TPackageLoader.UpdateImageList;
const
  DefaultImageIndex = 1;
var
  i, CompIndex: Integer;
  Bmp: TBitmap;
  h: THandle;
  cName: string;
  C: TComponentClass;
begin
  ImageList.Clear;
  for i := 0 to ComponentCount - 1 do
    Components[i].ImageIndex := DefaultImageIndex;

  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(HInstance, 'TSELECTOR');
    Bmp.Width := ImageList.Width;
    Bmp.Height := ImageList.Height;
    ImageList.Add(Bmp, nil);

    Bmp.Assign(nil); // fixes GDI resource leak
    Bmp.LoadFromResourceName(HInstance, 'DEFAULTCOMPONENT');
    ImageList.Add(Bmp, nil);

    for i := 0 to PackageCount - 1 do
    begin
      h := Packages[i].Handle;
      for CompIndex := 0 to Packages[i].ComponentCount - 1 do
      begin
        C := Packages[i].Components[CompIndex].ComponentClass;
        while (C <> nil) do
        begin
          cName := C.ClassName;
          if FindResource(h, PChar(cName), RT_BITMAP) <> 0 then
            Break;
          if C.ClassParent.InheritsFrom(TComponent) then
            C := TComponentClass(C.ClassParent)
          else
            C := nil;
        end;
        if C <> nil then
        begin
          Bmp.Assign(nil); // fixes GDI resource leak
          Bmp.LoadFromResourceName(h, C.ClassName);
          Bmp.Width := ImageList.Width;
          Bmp.Height := ImageList.Height;
          Packages[i].Components[CompIndex].ImageIndex := ImageList.Add(Bmp, nil);
        end;
      end;
    end;
  finally
    Bmp.Free;
  end;
end;

{ TPackage }

procedure PackageInfoProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);
var
  RegisterProc: procedure;
  Package: TPackage;
begin
  Package := TPackage(Param);
  case NameType of
    ntRequiresPackage:
      Package.FRequires.Add(Name);

    {$IFDEF COMPILER6_UP}
    ntDcpBpiName:
      Package.FDcpBpiName := Name;
    {$ENDIF COMPILER6_UP}

    ntContainsUnit:
      begin
        if (Flags in [0, ufWeakPackageUnit]) and (CompareText(Name, 'SysInit') <> 0) then
        begin
          @RegisterProc := GetProcAddress(Package.Handle, PChar(Format('@%s@Register$qqrv', [UpName(Name)])));
          if Assigned(RegisterProc) then
            RegisterProc;
        end;
        if Flags and (ufMainUnit or ufPackageUnit) <> 0 then
          Package.FName := Name
        else
          Package.FUnits.Add(TUnit.Create(Name, Flags));
      end;
  end;
end;

constructor TPackage.Create(AHandle: THandle; APackageLoader: TPackageLoader);
var
  Buf: array[0..MAX_PATH] of Char;
begin
  inherited Create;
  FHandle := AHandle;
  FPackageLoader := APackageLoader;
  FRequires := TStringList.Create;
  FUnits := TInterfaceList.Create;
  FComponents := TInterfaceList.Create;
  FActions := TInterfaceList.Create;

  FPackageLoader.FCurrentPackage := Self;
  // fill data fields
  GetModuleFileName(AHandle, Buf, SizeOf(Buf));
  FDescription := GetPackageDescription(Buf);
  GetPackageInfo(FHandle, Self, FFlags, PackageInfoProc);
end;

destructor TPackage.Destroy;
begin
  FRequires.Free;
  FActions.Free;
  FComponents.Free;
  FUnits.Free;
  UnRegisterModuleClasses(FHandle);
  UnloadPackage(FHandle);
  inherited Destroy;
end;

function TPackage.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

function TPackage.GetComponents(Index: Integer): IComponentItem;
begin
  Result := FComponents[Index] as IComponentItem;
end;

function TPackage.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

function TPackage.GetActions(Index: Integer): IActionItem;
begin
  Result := FActions[Index] as IActionItem;
end;

function TPackage.GetDcpBpiName: string;
begin
  Result := FDcpBpiName;
end;

function TPackage.GetFlags: Integer;
begin
  Result := FFlags;
end;

function TPackage.GetHandle: THandle;
begin
  Result := FHandle;
end;

function TPackage.GetIsRunOnly: Boolean;
begin
  Result := (Flags and pfRunOnly <> 0) and (Flags and pfDesignOnly = 0);
end;

function TPackage.GetName: string;
begin
  Result := FName;
end;

function TPackage.GetRequireCount: Integer;
begin
  Result := FRequires.Count;
end;

function TPackage.GetRequires(Index: Integer): string;
begin
  Result := FRequires[Index];
end;

function TPackage.GetUnitCount: Integer;
begin
  Result := FUnits.Count;
end;

function TPackage.GetUnits(Index: Integer): IUnit;
begin
  Result := FUnits[Index] as IUnit;
end;

function TPackage.GetDescription: string;
begin
  Result := FDescription;
end;

{ TUnit }

constructor TUnit.Create(const AName: string; AFlags: Integer);
begin
  inherited Create;
  FName := AName;
  FFlags := AFlags;
end;

function TUnit.GetFlags: Integer;
begin
  Result := FFlags;
end;

function TUnit.GetName: string;
begin
  Result := FName;
end;

{ TComponentItem }

constructor TComponentItem.Create(const APalette: string;
  AComponentClass: TComponentClass; AImageIndex: Integer);
begin
  inherited Create;
  FPalette := APalette;
  FComponentClass := AComponentClass;
  FImageIndex := AImageIndex;
end;

function TComponentItem.GetComponentClass: TComponentClass;
begin
  Result := FComponentClass;
end;

function TComponentItem.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

function TComponentItem.GetPalette: string;
begin
  Result := FPalette;
end;

function TComponentItem.GetUnitName: string;
begin
  Result := GetTypeData(ComponentClass.ClassInfo).UnitName;
end;

procedure TComponentItem.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
end;

{ TActionItem }

constructor TActionItem.Create(const ACategory: string; AActionClass: TBasicActionClass;
  AResource: TComponentClass);
begin
  inherited Create;
  FCategory := ACategory;
  FActionClass := AActionClass;
  FResource := AResource;
end;

function TActionItem.GetActionClass: TBasicActionClass;
begin
  Result := FActionClass;
end;

function TActionItem.GetCategory: string;
begin
  Result := FCategory;
end;

function TActionItem.GetResource: TComponentClass;
begin
  Result := FResource;
end;

function TActionItem.GetUnitName: string;
begin
  Result := GetTypeData(ActionClass.ClassInfo).UnitName;
end;

end.
