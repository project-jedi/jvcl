// simple unit that defines save/store interface
unit PersistSettings;

interface
uses
  Classes, IniFiles;

type
  IPersistSettings = interface
  ['{74727C9C-50F6-4C3A-8051-5CC5E64E9EB7}']
    procedure Load(Storage:TCustomIniFile);
    procedure Save(Storage:TCustomIniFile);
  end;

// save a component and any sub-component that implements the IPersistSettings interface
procedure SaveComponents(Root:TComponent;Storage:TCustomIniFile);
// load a component and any sub-component that implements the IPersistSettings interface
procedure LoadComponents(Root:TComponent;Storage:TCustomIniFile);

implementation
uses
  SysUtils;

procedure SaveComponents(Root:TComponent;Storage:TCustomIniFile);
var i:integer;PS:IPersistSettings;
begin
  if Supports(Root,IPersistSettings,PS) then
    PS.Save(Storage);
  for i := 0 to Root.ComponentCount-1 do
    SaveComponents(Root.Components[i],Storage);
end;

procedure LoadComponents(Root:TComponent;Storage:TCustomIniFile);
var i:integer;PS:IPersistSettings;
begin
  if Supports(Root,IPersistSettings,PS) then
    PS.Load(Storage);
  for i := 0 to Root.ComponentCount-1 do
    LoadComponents(Root.Components[i],Storage);
end;

end.
