{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Ralf Grenzing [ralfgspam@gmx.de]
                  Uwe Rupprecht [uwe-rupprecht@gmx.de]

 Contributor(s): Michael Beck (mbeck1@compuserve.com)
 Settings part based on work of Angus Johnson - ajohnson@rpi.net.au
                                                                  
 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may  
 not use this file except in compliance with the License. You may 
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit testClasses;

interface

uses
  Classes;

type
  TDocuments = class;
  Document = class;

  TCatalogue = class(TPersistent)
  private
    FDocuments: TDocuments;
    FFooter: string;
    FHeader: string;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Header: string read FHeader write FHeader;
    property Documents: TDocuments read FDocuments write FDocuments;
    property Footer: string read FFooter write FFooter;
  end;

  Document = class(TCollectionItem)
  private
    FDocIndex: integer;
    FTitle: string;
    FAuthor: string;
    FISBN: string;
    FPublisher: string;
    FPublicDate: string;
    FAdditionalInformation: string;
    FVersion: string;
  published
    property DocIndex: integer read FDocIndex write FDocIndex;
    property ISBN: string read FISBN write FISBN;
    property Author: string read FAuthor write FAuthor;
    property Title: string read FTitle write FTitle;
    property Publisher: string read FPublisher write FPublisher;
    property Version: string read FVersion write FVersion;
    property PublicDate: string read FPublicDate write FPublicDate;
    property AdditionalInformation: string read FAdditionalInformation write FAdditionalInformation;
  end;

  TDocuments = class(TCollection)
  private
    function GetItem(Index: Integer): Document;
    procedure SetItem(Index: Integer; const Value: Document);
  public
    function  Add: Document;
    function Insert(Index: Integer): Document;
    property Items[Index: Integer]: Document read GetItem  write SetItem; default;
  end;

implementation

{ Document }

{ TDocuments }

function TDocuments.Add: Document;
begin
  Result := Document(inherited Add);
end;

function TDocuments.GetItem(Index: Integer): Document;
begin
  Result := Document(inherited Items[Index]);
end;

function TDocuments.Insert(Index: Integer): Document;
begin
  Result := Document(inherited Insert(Index));
end;

procedure TDocuments.SetItem(Index: Integer; const Value: Document);
begin
  Items[Index].Assign(Value);
end;

{ TCatalogue }

constructor TCatalogue.Create(AOwner: TComponent);
begin
  Documents := TDocuments.Create(Document);
end;

destructor TCatalogue.Destroy;
begin
  inherited Destroy;
  Documents.Free;
end;


end.
