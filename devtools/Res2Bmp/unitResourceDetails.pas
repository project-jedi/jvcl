(*======================================================================*
 | unitResourceDetails                                                  |
 |                                                                      |
 | Ultra-light classes to wrap resources and resource modules.          |
 |                                                                      |
 | TResourceModule is an abstract base class for things that can        |
 | provide lists of resources - eg. .RES files, modules, etc.           |
 |                                                                      |
 | TResourceDetails is a base class for resources.                      |
 |                                                                      |
 | ... and here's the neat trick...                                     |
 |                                                                      |
 | Call the class function TResourceDetails.CreateResourceDetails to    |
 | create an instance of the appropriate registered TResourceDetails    |
 | descendant                                                           |
 |                                                                      |
 | ** Gold code **                                                      |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001                                      |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 *======================================================================*)


unit unitResourceDetails;

interface

uses Windows, Classes, SysUtils;

type

TResourceDetails = class;
TResourceDetailsClass = class of TResourceDetails;

//======================================================================
// TResourceModule class

TResourceModule = class
private
  fDirty : Boolean;
  function GetDirty: Boolean;
protected
  function GetResourceCount: Integer; virtual; abstract;
  function GetResourceDetails(idx: Integer): TResourceDetails; virtual; abstract;

public
  procedure DeleteResource (idx : Integer); virtual;
  procedure InsertResource (idx : Integer; details : TResourceDetails); virtual;
  function AddResource (details : TResourceDetails) : Integer; virtual;
  function IndexOfResource (details : TResourceDetails) : Integer; virtual; abstract;
  function GetUniqueResourceName (const tp : string) : string;

  procedure SaveToStream (stream : TStream); virtual;
  procedure LoadFromStream (stream : TStream); virtual;

  procedure SaveToFile (const FileName : string); virtual;
  procedure LoadFromFile (const FileName : string); virtual;
  procedure SortResources; virtual;

  function FindResource (const tp, Name : string; ALanguage : Integer) : TResourceDetails;

  property ResourceCount : Integer read GetResourceCount;
  property ResourceDetails [idx : Integer] : TResourceDetails read GetResourceDetails;
  property Dirty : Boolean read GetDirty write fDirty;
  procedure ClearDirty;
end;

//======================================================================
// TResourceDetails class

TResourceDetails = class
private
  fParent : TResourceModule;
  fData : TMemoryStream;
  fCodePage : Integer;
  fResourceLanguage: LCID;
  fResourceName: string;
  fResourceType: string;

  fMemoryFlags : word;                    // Resource memory flags
  fDataVersion, fVersion : DWORD;         // Resource header version info
  fCharacteristics : DWORD;
  fTag: LongInt;
  fDirty : Boolean;
                                         // Resource header characteristics

protected
  constructor Create (AParent : TResourceModule; ALanguage : Integer; const AName, AType : string; ASize : Integer; AData : pointer); virtual;
  procedure InitNew; virtual;
  procedure SetResourceName(const Value: string); virtual;
  class function SupportsRCData (const AName : string; Size : Integer; data : Pointer) : Boolean; virtual;
  class function SupportsData (Size : Integer; data : Pointer) : Boolean; virtual;
public
  class function CreateResourceDetails (AParent : TResourceModule; ALanguage : Integer; const AName, AType : string; ASize : Integer; AData : pointer) : TResourceDetails;
  class function GetBaseType : string; virtual;

  constructor CreateNew (AParent : TResourceModule; ALanguage : Integer; const AName : string); virtual;
  destructor Destroy; override;
  procedure BeforeDelete; virtual;

  procedure ChangeData (newData : TMemoryStream); virtual;

  property Parent : TResourceModule read fParent;
  property Data : TMemoryStream read fData;
  property ResourceName : string read fResourceName write SetResourceName;
  property ResourceType : string read fResourceType;
  property ResourceLanguage : LCID read fResourceLanguage write fResourceLanguage;

  property CodePage : Integer read fCodePage write fCodePage;
  property Characteristics : DWORD read fCharacteristics write fCharacteristics;
  property Version : DWORD read fVersion write fDataVersion;
  property DataVersion : DWORD read fDataVersion write fDataVersion;
  property MemoryFlags : WORD read fMemoryFlags write fMemoryFlags;

  property Dirty : Boolean read fDirty write fDirty;

  property Tag : LongInt read fTag write fTag;
end;

//======================================================================
// Global function definitions

procedure RegisterResourceDetails (resourceClass : TResourceDetailsClass);
procedure UnRegisterResourceDetails (resourceClass : TResourceDetailsClass);
function ResourceWideCharToStr(var wstr : PWideChar) : string;
procedure ResourceStrToWideChar (const s : string; var p : PWideChar);
function ResourceNameToInt (const s : string) : Integer;
function CompareDetails (p1, p2 : Pointer) : Integer;

implementation

var
  registeredResourceDetails : array of TResourceDetailsClass;
  registeredResourceDetailsCount : Integer = 0;

resourcestring
  rstNoBaseType = 'Can''t register resource details class with no base type';
  rstNoStreaming = 'Module doesn''t support streaming';

(*----------------------------------------------------------------------*
 | procedure RegisterResourceDetails                                    |
 |                                                                      |
 | Add a class, derived from TResourceDetails, to the list of           |
 | registered resource details classes                                  |
 *----------------------------------------------------------------------*)
procedure RegisterResourceDetails (resourceClass : TResourceDetailsClass);
begin
  if Length (registeredResourceDetails) = registeredResourceDetailsCount then
    SetLength (registeredResourceDetails, Length (registeredResourceDetails) + 10);

  registeredResourceDetails [registeredResourceDetailsCount] := resourceClass;

  Inc (registeredResourceDetailsCount)
end;

(*----------------------------------------------------------------------*
 | procedure UnRegisterResourceDetails                                  |
 |                                                                      |
 | Remove a class, derived from TResourceDetails, from the list of      |
 | registered resource details classes                                  |
 *----------------------------------------------------------------------*)
procedure UnRegisterResourceDetails (resourceClass : TResourceDetailsClass);
var
  i : Integer;
begin
  i := 0;
  while i < registeredResourceDetailsCount do
    if registeredResourceDetails [i] = resourceClass then
    begin
      if i < Length (registeredResourceDetails) - 1 then
        Move (registeredResourceDetails [i + 1], registeredResourceDetails [i], (Length (registeredResourceDetails) - i - 1) * sizeof (TResourceDetailsClass));

      Dec (registeredResourceDetailsCount)
    end
    else
      Inc (i)
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideCharToStr ()                                         |
 |                                                                            |
 | Convert Pascal-style WideChar array to a string                            |
 |                                                                            |
 | Parameters:                                                                |
 |   WStr : PWChar             The characters                                 |
 *----------------------------------------------------------------------------*)
function ResourceWideCharToStr(var wstr : PWideChar) : string;
var
  len : word;
begin
  len := word (wstr^);
  SetLength (result, len);
  Inc (wstr);
  WideCharToMultiByte(CP_ACP, 0, WStr, Len, PChar (Result), Len + 1, nil, nil);
  Inc (wstr, len);
  result := PChar (result);
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceStrToWideChar ()                                         |
 |                                                                            |
 | Convert a string to a Pascal style Wide char array                         |
 |                                                                            |
 | Parameters:                                                                |
 |   s : string                The string                                     |
 |   var p : PWideChar         [in]  Points to the start of the receiving buf |
 |                             [out] Points after the characters.             |
 *----------------------------------------------------------------------------*)
procedure ResourceStrToWideChar (const s : string; var p : PWideChar);
var
  buffer : PWideChar;
  len, size : word;
begin
  len := Length (s);
  size := (Length (s) + 1) * sizeof (WideChar);
  GetMem (buffer, size);
  try
    MultiByteToWideChar (CP_ACP, 0, PChar (s), -1, buffer, size);
    p^ := WideChar (len);
    Inc (p);
    Move (buffer^, p^, len * sizeof (WideChar));
    Inc (p, len)
  finally
    FreeMem (buffer)
  end
end;

(*----------------------------------------------------------------------*
 | procedure ResourceNameToInt                                          |
 |                                                                      |
 | Get integer value of resource name (or type).  Return -1 if it's     |
 | not numeric.                                                         |
 *----------------------------------------------------------------------*)
function ResourceNameToInt (const s : string) : Integer;
var
  isNumeric : Boolean;
  i : Integer;
begin
  isNumeric := Length (s) > 0;
  for i := 1 to Length (s) do
    if not (s [i] in ['0'..'9']) then
    begin
      isNumeric := False;
      break
    end;

  if isNumeric then
    Result := StrToInt (s)
  else
    Result := -1
end;

(*----------------------------------------------------------------------*
 | function CompareDetails                                              |
 |                                                                      |
 | 'Compare' function used when sorting resources.  p1 and p2 must be   |
 | TResourceDetails references.  Returns > 0 if details at p1 are >     |
 | details at p2.                                                       |
 |                                                                      |
 | *  Compare resource types.  If they match then compare names.        |
 | *  'Integer' ids or names must come *after* non integer ids or names.|
 *----------------------------------------------------------------------*)
function CompareDetails (p1, p2 : Pointer) : Integer;
var
  d1 : TResourceDetails;
  d2 : TResourceDetails;
  i1, i2 : Integer;
begin
  d1 := TResourceDetails (p1);
  d2 := TResourceDetails (p2);

  i1 := ResourceNameToInt (d1.ResourceType);
  i2 := ResourceNameToInt (d2.ResourceType);

  if i1 >= 0 then
    if i2 >= 0 then
      Result := i1 - i2         // Compare two integer ids
    else
      Result := 1               // id1 is int, so it's greater than non-int id2
  else
    if i2 >= 0 then
      Result := -1              // id2 is int, so it's less than non-int id1
    else
                                // Compare two string resource ids
      Result := CompareText (d1.ResourceType, d2.ResourceType);

  if Result = 0 then            // If they match, do the same with the names
  begin
    i1 := ResourceNameToInt (d1.ResourceName);
    i2 := ResourceNameToInt (d2.ResourceName);

    if i1 >= 0 then
      if i2 >= 0 then
        Result := i1 - i2
      else
        Result := 1
    else
      if i2 >= 0 then
        Result := -1
      else
        Result := CompareText (d1.ResourceName, d2.ResourceName)
  end
end;

{ TResourceDetails }

(*----------------------------------------------------------------------*
 | TResourceDetails.BeforeDelete                                        |
 |                                                                      |
 | Can override this to clear up before deleting.  Eg. deleting an      |
 | icon removes it from the icon group it's in.  Deleting an icon group |
 | removes the individual icon resources, etc.                          |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.BeforeDelete;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.ChangeData                                          |
 |                                                                      |
 | Change all the data.  Handy for implementing 'undo', etc.            |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.ChangeData(newData: TMemoryStream);
begin
  fData.Clear;
  fData.CopyFrom (newData, 0);
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Create                                              |
 |                                                                      |
 | Raw - protected - constructor for resource details.                  |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.Create(AParent: TResourceModule; ALanguage: Integer; const AName, AType: string; ASize: Integer;
  AData: pointer);
begin
  fParent := AParent;
  fResourceLanguage := ALanguage;
  fResourceName := AName;
  fResourceType := AType;
  fData := TMemoryStream.Create;
  fData.Write (AData^, ASize)
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateNew                                           |
 |                                                                      |
 | Constructor to be used when adding new resources to a module.        |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const aName : string);
begin
  fParent := AParent;
  fResourceLanguage := ALanguage;
  fResourceName := AName;
  fResourceType := GetBaseType;
  if Assigned (AParent) then
    AParent.AddResource (Self);
  fData := TMemoryStream.Create;
  InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateResourceDetails                               |
 |                                                                      |
 | Create a class derived from TResourceDetals that reflects the 'Type' |
 | If no matching class is registered, create a base 'TResourceDetails' |
 | class.    (Ha!  Try doing *that* in C++ ! )                          |
 *----------------------------------------------------------------------*)
class function TResourceDetails.CreateResourceDetails(
  AParent: TResourceModule; ALanguage: Integer; const AName,
  AType: string; ASize: Integer; AData: pointer): TResourceDetails;
var
  i : Integer;
begin
  result := Nil;

  if (Length (AType) > 0) and (AType [1] in ['0'..'9']) then
  try
    for i := 0 to registeredResourceDetailsCount - 1 do
      if registeredResourceDetails [i].GetBaseType = AType then
      begin
        if (AType <> IntToStr (Integer (RT_RCDATA))) or registeredResourceDetails [i].SupportsRCData (AName, ASize, AData) then
        begin
          result := registeredResourceDetails [i].Create (AParent, ALanguage, AName, AType, ASize, AData);
          break
        end
      end;
  except
  end;

  if Result = nil then
  try
    for i := 0 to registeredResourceDetailsCount - 1 do
      if registeredResourceDetails [i].SupportsData (ASize, AData) then
      begin
        result := registeredResourceDetails [i].Create (AParent, ALanguage, AName, AType, ASize, AData);
        break
      end;
  except
  end;

  if result = Nil then
    result := TResourceDetails.Create (AParent, ALanguage, AName, AType, ASize, AData)
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Destroy                                             |
 *----------------------------------------------------------------------*)
destructor TResourceDetails.Destroy;
begin
  fData.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.GetBaseType                                         |
 |                                                                      |
 | Return the base type for the resource details.  This is overridden   |
 | in derived classes.                                                  |
 *----------------------------------------------------------------------*)
class function TResourceDetails.GetBaseType: string;
begin
  Result := '0';
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.InitNew                                             |
 |                                                                      |
 | Override this to initialize a new resource being added to a module.  |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.InitNew;
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SetResourceName                                     |
 |                                                                      |
 | Set the resource name.                                               |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.SetResourceName(const Value: string);
begin
  fResourceName := Value;
  fDirty := True
end;

{ TResourceModule }

(*----------------------------------------------------------------------*
 | TResourceModule.LoadFromFile                                         |
 |                                                                      |
 | Load from file.  This can be overriden but usually isn't as it       |
 | relies on LoadFromStream, which must be.                             |
 *----------------------------------------------------------------------*)
function TResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  result := -1
  // Stub
end;

procedure TResourceModule.ClearDirty;
var
  i : Integer;
begin
  fDirty := False;
  for i := 0 to ResourceCount - 1 do
    ResourceDetails [i].Dirty := False
end;

(*----------------------------------------------------------------------*
 | TResourceModule.DeleteResource                                       |
 |                                                                      |
 | Must be overridden to remove the resource details object from        |
 | wherever it's stored.  The overriding method must call               |
 | inherited                                                            |
 *----------------------------------------------------------------------*)
procedure TResourceModule.DeleteResource(idx: Integer);
begin
  fDirty := True;
  ResourceDetails [idx].BeforeDelete;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.FindResource                                         |
 |                                                                      |
 | Find a resource with a given type/name                               |
 *----------------------------------------------------------------------*)
function TResourceModule.FindResource(const tp,
  Name: string; ALanguage : Integer): TResourceDetails;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ResourceCount - 1 do
    if (ResourceDetails [i].fResourceType = tp) and (ResourceDetails [i].fResourceName = Name) and (Integer (ResourceDetails [i].fResourceLanguage) = ALanguage) then
    begin
      Result := ResourceDetails [i];
      break
    end;

  if not Assigned (result) then
    for i := 0 to ResourceCount - 1 do
      if (ResourceDetails [i].fResourceType = tp) and (ResourceDetails [i].fResourceName = Name) and (ResourceDetails [i].fResourceLanguage = 0) then
      begin
        Result := ResourceDetails [i];
        break
      end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetDirty                                             |
 |                                                                      |
 | Returns true if the module or it's resources are 'dirty'             |
 |                                                                      |
 | nb. fDirty is only set if resources have been deleted.               |
 |     After adding a resource make sure the resource's Dirty is set to |
 |     true.                                                            |
 *----------------------------------------------------------------------*)
function TResourceModule.GetDirty: Boolean;
var
  i : Integer;
begin
  Result := fDirty;
  if not fDirty then
    for i := 0 to ResourceCount - 1 do
      if ResourceDetails [i].Dirty then
      begin
        Result := True;
        break
      end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetUniqueResourceName                                |
 |                                                                      |
 | Generate a unique resource name for a given type.  Names start at    |
 | 1 (though string lists downgrade that to '0')                        |
 *----------------------------------------------------------------------*)
function TResourceModule.GetUniqueResourceName(const tp: string): string;
var
  i : Integer;
  n, n1 : Integer;
  details : TResourceDetails;
begin
  n := 0;

  for i := 0 to ResourceCount - 1 do
  begin
    details := ResourceDetails [i];
    if details.ResourceType = tp then
    begin
      n1 := ResourceNametoInt (details.ResourceName);
      if n1 > n then
        n := n1
    end
  end;

  Result := IntToStr (n + 1);
end;

procedure TResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceModule.LoadFromFile                                         |
 |                                                                      |
 | Load from a file.  Not usually overriden.                            |
 *----------------------------------------------------------------------*)
procedure TResourceModule.LoadFromFile(const FileName: string);
var
  s : TFileStream;
begin
  s := TFileStream.Create (FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (s);
  finally
    s.Free
  end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.SaveToFile                                           |
 |                                                                      |
 | Save to file.  This can be overriden but usually isn't as it         |
 | relies on SaveToStream, which must be.                               |
 *----------------------------------------------------------------------*)

procedure TResourceModule.LoadFromStream(stream: TStream);
begin
  raise Exception.Create (rstNoStreaming);
end;

procedure TResourceModule.SaveToFile(const FileName: string);
var
  s : TFileStream;
begin
  s := TFileStream.Create (FileName, fmCreate);
  try
    SaveToStream (s);
    ClearDirty
  finally
    s.Free
  end
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support a custom resource class, where you can  |
 | determine the custom class from the data - eg. RIFF data, etc.       |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
begin
  Result := False; // stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support RC data where you can determine the     |
 | type from the data and name - eg. the Delphi splash screen JPEG      |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsRCData(const AName: string;
  Size: Integer; data: Pointer): Boolean;
begin
  Result := False; // stub
end;

procedure TResourceModule.SaveToStream(stream: TStream);
begin
  raise Exception.Create (rstNoStreaming);
end;

procedure TResourceModule.SortResources;
begin
// Stub
end;

end.
