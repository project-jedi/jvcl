{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBDotNetCtrls.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBDotNetControls;

{$I jvcl.inc}

{$IFDEF DelphiPersonalEdition}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{$ELSE}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Controls, DBCtrls,
  JvDBFindEdit, JVCLVer, JvDotNetUtils;

type
  TJvDotNetDBEdit = class(TDBEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvDotNetDBListBox = class(TDBListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvDotNetDBLookupListBox = class(TDBLookupListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvDotNetDBMemo = class(TDBMemo)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvDotNetDBRichEdit = class(TDBRichEdit)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvDotNetDBFindEdit = class(TJvDBFindEdit)
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

//=== { TJvDotNetDBEdit } ====================================================

constructor TJvDotNetDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDBEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDBEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetDBListBox } =================================================

constructor TJvDotNetDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDBListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDBListBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetDBLookupListBox } ===========================================

constructor TJvDotNetDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDBLookupListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDBLookupListBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetDBMemo } ====================================================

constructor TJvDotNetDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDBMemo.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDBMemo.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetDBRichEdit } ================================================

constructor TJvDotNetDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDBRichEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDBRichEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== { TJvDotNetDBFindEdit } ================================================

constructor TJvDotNetDBFindEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

destructor TJvDotNetDBFindEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

procedure TJvDotNetDBFindEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

{$ENDIF DelphiPersonalEdition}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.