{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataEmbedded.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse ,dejoy.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

 Added TJvComponentEmbedded and TJvPersistentEmbedded from dejoy 2004-07-30:
 Override DefineUnpublishedProperties to define property in DefineUnpublishedProperties,

  If you want to define a general property, use DoDefineProperty,
  If you want to define a binary property, use DoDefineBinaryProperty.
  Override ReadUnpublished and WriteUnpublished to read or write non-published properties.
  You can use DefinePropertyIs in ReadUnpublished or WriteUnpublished  to detect which
  property is being processed.

  If you have many unpublished properties to process, you don't need to write a lot of
  WriteXXX or ReadXXX procedures to process them. Instead, just override
  DefineUnpublishedProperties, ReadUnpublished and WriteUnpublished

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDataEmbedded;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  JVCLXVer;

type 
  TJvPersistentEmbedded = class(TInterfacedPersistent)
  private
    FFiler: TFiler;
    FFilerTag: string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefineUnpublishedProperties(Filer: TFiler); virtual;
    function DefinePropertyIs(const Name: string): Boolean; dynamic;
    procedure DoDefineProperty(const Name: string; HasData: Boolean); dynamic;
    procedure DoDefineBinaryProperty(const Name: string; HasData: Boolean); dynamic;
    procedure ReadUnpublished(Reader: TReader); overload; dynamic;
    procedure WriteUnpublished(Writer: TWriter); overload; dynamic;
    procedure ReadUnpublished(Stream: TStream); overload; dynamic;
    procedure WriteUnpublished(Stream: TStream); overload; dynamic;
  public
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
  end;

  TJvComponentEmbedded = class(TComponent)
  private
    FFiler: TFiler;
    FFilerTag: string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefineUnpublishedProperties(Filer: TFiler); virtual;
    function DefinePropertyIs(const Name: string): Boolean; dynamic;
    procedure DoDefineProperty(const Name: string; HasData: Boolean); dynamic;
    procedure DoDefineBinaryProperty(const Name: string; HasData: Boolean); dynamic;
    procedure ReadUnpublished(Reader: TReader); overload; dynamic;
    procedure WriteUnpublished(Writer: TWriter); overload; dynamic;
    procedure ReadUnpublished(Stream: TStream); overload; dynamic;
    procedure WriteUnpublished(Stream: TStream); overload; dynamic;
  public
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
  end;

  TJvDataEmbedded = class(TJvComponentEmbedded)
  private
    FAboutJVCL: TJVCLAboutInfo; // (ahuser) removed JvComponent dependency for easy CLX usage
    FStream: TMemoryStream;
    FOnLoading: TNotifyEvent;
    FOnSaved: TNotifyEvent;
    FOnLoaded: TNotifyEvent;
    FOnSaving: TNotifyEvent;
    FOnChange: TNotifyEvent;
    function GetSize: Integer;
    function GetStream: TStream;
    procedure SetStream(const Value: TStream);
    procedure SetSize(const Value: Integer);
  protected
    procedure DefineUnpublishedProperties(Filer: TFiler); override;
    procedure ReadUnpublished(Stream: TStream); override;
    procedure WriteUnpublished(Stream: TStream); override;
    procedure DoLoading; virtual;
    procedure DoLoaded; virtual;
    procedure DoSaving; virtual;
    procedure DoSaved; virtual;
    procedure Change; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DataLoadFromFile(const FileName: TFileName);
    procedure DataLoadFromStream(Stream: TStream);
    procedure DataSaveToFile(const FileName: TFileName);
    procedure DataSaveToStream(Stream: TStream);
    property Size: Integer read GetSize write SetSize;
    property Data: TStream read GetStream write SetStream;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property OnLoading: TNotifyEvent read FOnLoading write FOnLoading;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnSaving: TNotifyEvent read FOnSaving write FOnSaving;
    property OnSaved: TNotifyEvent read FOnSaved write FOnSaved;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  cEmbeddedData = 'EmbeddedData';

//=== { TJvDataEmbedded } ====================================================

constructor TJvDataEmbedded.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStream := TMemoryStream.Create;
end;

destructor TJvDataEmbedded.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TJvDataEmbedded.GetSize: Integer;
begin
  Result := FStream.Size;
end;

function TJvDataEmbedded.GetStream: TStream;
begin
  Result := FStream;
  Result.Position := 0;
end;

procedure TJvDataEmbedded.SetStream(const Value: TStream);
begin
  FStream.Clear;
  if Value <> nil then
    FStream.CopyFrom(Value, Value.Size - Value.Position);
  Change;
end;

procedure TJvDataEmbedded.DataSaveToFile(const FileName: TFileName);
begin
  DoSaving;
  FStream.SaveToFile(FileName);
  DoSaved;
end;

procedure TJvDataEmbedded.DataSaveToStream(Stream: TStream);
begin
  DoSaving;
  Stream.CopyFrom(FStream, 0);
  DoSaved;
end;

procedure TJvDataEmbedded.SetSize(const Value: Integer);
begin
  FStream.SetSize(Value);
  Change;
end;

procedure TJvDataEmbedded.DefineUnpublishedProperties(Filer: TFiler);
begin
  inherited DefineUnpublishedProperties(Filer);
  DoDefineBinaryProperty(cEmbeddedData, FStream.Size > 0);
end;

procedure TJvDataEmbedded.ReadUnpublished(Stream: TStream);
var
  I: Integer;
begin
  if DefinePropertyIs(cEmbeddedData) then
  begin
    Stream.Read(I, SizeOf(I));
    FStream.Clear;
    FStream.Size := I;
    Stream.Read(FStream.Memory^, I);
  end;
end;

procedure TJvDataEmbedded.WriteUnpublished(Stream: TStream);
var
  I: Integer;
begin
  if DefinePropertyIs(cEmbeddedData) then
  begin
    I := FStream.Size;
    Stream.Write(I, SizeOf(I));
    Stream.Write(FStream.Memory^, I);
  end;
end;

procedure TJvDataEmbedded.DataLoadFromFile(const FileName: TFileName);
begin
  DoLoading;
  FStream.LoadFromFile(FileName);
  DoLoaded;
end;

procedure TJvDataEmbedded.DataLoadFromStream(Stream: TStream);
begin
  DoLoading;
  FStream.CopyFrom(Stream, 0);
  DoLoaded;
end;

procedure TJvDataEmbedded.DoLoaded;
begin
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
  Change;
end;

procedure TJvDataEmbedded.DoLoading;
begin
  if Assigned(FOnLoading) then
    FOnLoading(Self);
end;

procedure TJvDataEmbedded.DoSaved;
begin
  if Assigned(FOnSaved) then
    FOnSaved(Self);
end;

procedure TJvDataEmbedded.DoSaving;
begin
  if Assigned(FOnSaving) then
    FOnSaving(Self);
end;

procedure TJvDataEmbedded.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//=== { TJvComponentEmbedded } ===============================================

procedure TJvComponentEmbedded.DefineProperties(Filer: TFiler);
begin
  FFiler := Filer;
  inherited DefineProperties(Filer);
  DefineUnpublishedProperties(Filer);
end;

function TJvComponentEmbedded.DefinePropertyIs(const Name: string): Boolean;
begin
  Result := AnsiSameText(FFilerTag, Name);
end;

procedure TJvComponentEmbedded.DefineUnpublishedProperties(Filer: TFiler);
begin
//
end;

procedure TJvComponentEmbedded.DoDefineBinaryProperty(const Name: string; HasData: Boolean);
begin
  FFilerTag := Name;
  if Assigned(FFiler) and (Name <> '') then
    FFiler.DefineBinaryProperty(Name, ReadUnpublished, WriteUnpublished, HasData);
end;

procedure TJvComponentEmbedded.DoDefineProperty(const Name: string; HasData: Boolean);
begin
  FFilerTag := Name;
  if Assigned(FFiler) and (Name <> '') then
    FFiler.DefineProperty(Name, ReadUnpublished, WriteUnpublished, HasData);
end;

procedure TJvComponentEmbedded.ReadUnpublished(Reader: TReader);
begin

end;

procedure TJvComponentEmbedded.LoadFromStream(Stream: TStream);
begin
  if (Stream <> nil) and (Stream.Size > 0) then
    Stream.ReadComponent(Self);
end;

procedure TJvComponentEmbedded.ReadUnpublished(Stream: TStream);
begin

end;

procedure TJvComponentEmbedded.WriteUnpublished(Writer: TWriter);
begin

end;

procedure TJvComponentEmbedded.SaveToStream(Stream: TStream);
begin
  Stream.WriteComponent(Self);
end;

procedure TJvComponentEmbedded.WriteUnpublished(Stream: TStream);
begin

end;

//=== { TJvPersistentEmbedded } ==============================================

type
  TPersistentWrapper = class(TComponent)
  private
    FPersistent: TPersistent;
  published
    property Persistent: TPersistent read FPersistent write FPersistent;
  end;

procedure TJvPersistentEmbedded.DefineProperties(Filer: TFiler);
begin
  FFiler := Filer;
  inherited DefineProperties(Filer);
  DefineUnpublishedProperties(Filer);
end;

function TJvPersistentEmbedded.DefinePropertyIs(const Name: string): Boolean;
begin
  Result := AnsiSameText(FFilerTag, Name);
end;

procedure TJvPersistentEmbedded.DefineUnpublishedProperties(Filer: TFiler);
begin
//
end;

procedure TJvPersistentEmbedded.DoDefineBinaryProperty(const Name: string; HasData: Boolean);
begin
  FFilerTag := Name;
  if Assigned(FFiler) and (Name <> '') then
    FFiler.DefineBinaryProperty(Name, ReadUnpublished, WriteUnpublished, HasData);
end;

procedure TJvPersistentEmbedded.DoDefineProperty(const Name: string; HasData: Boolean);
begin
  FFilerTag := Name;
  if Assigned(FFiler) and (Name <> '') then
    FFiler.DefineProperty(Name, ReadUnpublished, WriteUnpublished, HasData);
end;

procedure TJvPersistentEmbedded.LoadFromStream(Stream: TStream);
var
  M: TPersistentWrapper;
begin
  if (Stream <> nil) and (Stream.Size > 0) then
  begin
    M := TPersistentWrapper.Create(nil);
    try
      M.Persistent := Self;
      Stream.ReadComponent(M);
      M.Persistent := nil;
    finally
      M.Free;
    end;
  end;
end;

procedure TJvPersistentEmbedded.ReadUnpublished(Stream: TStream);
begin
//
end;

procedure TJvPersistentEmbedded.ReadUnpublished(Reader: TReader);
begin
//
end;

procedure TJvPersistentEmbedded.SaveToStream(Stream: TStream);
var
  M: TPersistentWrapper;
begin
  M := TPersistentWrapper.Create(nil);
  try
    M.Persistent := Self;
    Stream.WriteComponent(M);
  finally
    M.Free;
  end;
end;

procedure TJvPersistentEmbedded.WriteUnpublished(Writer: TWriter);
begin

end;

procedure TJvPersistentEmbedded.WriteUnpublished(Stream: TStream);
begin

end;

end.


