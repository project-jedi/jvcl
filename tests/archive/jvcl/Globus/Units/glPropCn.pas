{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1999 Chudin A.V, FidoNet: 1246.16	  
  								  
  
 ===================================================================
 glPropCn Unit 08.1999	  	         component TglPropertyCenter
 ===================================================================
}
unit glPropCn;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TypInfo;

type

  TglProperties_ = (fupColor, fupFont, fupFontColor);
  TglProperties = set of TglProperties_;

  Tgl_Properties_ = (f_upColor, f_upFont, f_upFontColor);
  Tgl_Properties = set of Tgl_Properties_;

  TglPropertyCenter = class(TComponent)
  private
    FColorProperty       : TColor;
    FFontColorProperty   : TColor;
    FFontProperty        : TFont;
    FComponentList       : TStringList;
    FUseProperties       : TglProperties;
    FAutoApdate          : boolean;
    procedure SetColorProperty(Value: TColor);
    procedure SetFontColorProperty(Value: TColor);
    procedure SetFontProperty(Value: TFont);
    procedure UpdateProperties(Properties: Tgl_Properties);
  protected
    procedure Notification( Component: TComponent; Operation: TOperation );
    procedure Loaded; override;
  public
    CompList            : TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorProperty: TColor read FColorProperty write SetColorProperty;
    property FontColorProperty: TColor read FFontColorProperty write SetFontColorProperty;
    property FontProperty: TFont read FFontProperty write SetFontProperty;
    property ComponentList: TStringList read FComponentList write FComponentList;
    property UseProperties: TglProperties read FUseProperties write FUseProperties;
    property AutoApdate: boolean read FAutoApdate write FAutoApdate;
  end;

procedure Register;

implementation
uses glUtils, glTypes, geCList;
procedure Register;
begin
//  RegisterComponents('Proba', [Tgl3DColors]);
//  RegisterPropertyEditor( TypeInfo(TStringList), TglPropertyCenter, 'Resources', TglComponentListProperty );
end;

constructor TglPropertyCenter.Create(AOwner: TComponent);
begin
  inherited;
  ComponentList := TStringList.Create;
  CompList := TList.Create;
end;

destructor TglPropertyCenter.Destroy;
begin
  inherited;
  ComponentList.Free;
  CompList.Free;
end;

procedure TglPropertyCenter.Loaded;
var
  i: integer;
  Comp: TComponent;
begin
  inherited;

  for i := 0 to ComponentList.Count - 1 do
  begin
    Comp := Owner.FindComponent( ComponentList[i] );
    if Comp = nil then continue;
    CompList.Add(Comp);
    ComponentList.Add(Comp.Name);
  end;
end;

procedure TglPropertyCenter.Notification( Component: TComponent; Operation: TOperation );
begin
  if (Component <> Self)and(Operation = opRemove) then
    if CompList.IndexOf(Component) <> -1 then CompList.Delete(CompList.IndexOf(Component));
  inherited;
end;

procedure TglPropertyCenter.UpdateProperties(Properties: Tgl_Properties);
var
  i: integer;
  ColorPropInfo, FontPropInfo: PPropInfo;
begin
  for i:=0 to CompList.Count-1 do
  begin

    if f_upColor in Properties then
    begin
      ColorPropInfo := GetPropInfo( TComponent(CompList[i]).ClassInfo, 'Color');
      if ColorPropInfo <> nil then
        SetOrdProp( TComponent(CompList[i]), ColorPropInfo, FColorProperty );
    end;
{    if (fupFontColor in Properties)or(fupFont in Properties) then
    begin
      ColorPropInfo := GetPropInfo( TComponent(CompList[i]).ClassInfo, 'Font');
      if ColorPropInfo <> nil then
        if fupFontColor in Properties then
          SetOrdProp( TComponent(CompList[i]), PropInfo, FColorProperty );
    end;}
  end;

end;

procedure TglPropertyCenter.SetColorProperty(Value: TColor);
begin
  if FColorProperty = Value then exit;
  FColorProperty := Value;
  if FAutoApdate then UpdateProperties([f_upColor]);
end;

procedure TglPropertyCenter.SetFontColorProperty(Value: TColor);
begin
  if FFontColorProperty = Value then exit;
  FFontColorProperty := Value;
  if FAutoApdate then UpdateProperties([f_upFontColor]);
end;

procedure TglPropertyCenter.SetFontProperty(Value: TFont);
begin
  if FFontProperty = Value then exit;
  FFontProperty.Assign(Value);
  if FAutoApdate then UpdateProperties([f_upFont]);
end;


end.
