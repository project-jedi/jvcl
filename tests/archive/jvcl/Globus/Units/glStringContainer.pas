{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998,1999 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glStrCon Unit 07.1999	                component TglStringContainer
 ===================================================================
}
unit glStringContainer;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes;

type
  TOnReadItem = procedure(Sender: TObject; Index: integer) of object;

  TglStringContainer = class(TComponent)
  private
    FItems           : TStringList;
    FReadOnly        : boolean;
    FOnReadItem: TOnReadItem;
    function GetString( Index: integer ): string;
    procedure SetString(Index: integer; const Value: string);
    procedure SetItems(Value: TStringList);
    function GetCount: integer;
  public
    property Strings[Index: Integer]: string read GetString write SetString; default;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStringList read FItems write SetItems; 
    property Count: integer read GetCount;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property OnReadItem: TOnReadItem read FOnReadItem write FOnReadItem;
  end;

procedure Register;

implementation
uses glUtils, glTypes;

procedure Register;
begin
  RegisterComponents('Proba', [TglStringContainer]);
end;

constructor TglStringContainer.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TglStringContainer.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TglStringContainer.GetString(Index: integer): string;
begin
  if Assigned(FOnReadItem) then FOnReadItem(self, Index);
  Result := FItems[Index];
end;

procedure TglStringContainer.SetString(Index: integer; const Value: string);
begin
  if not FReadOnly then FItems[Index] := Value;
end;

procedure TglStringContainer.SetItems(Value: TStringList);
begin
  FItems.Assign(Value);
end;

function TglStringContainer.GetCount: integer;
begin
  Result := FItems.Count;
end;


end.
