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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDBDotNetControls;

{$IFDEF DelphiPersonalEdition}

interface

implementation

{$ELSE}

interface

uses
  Windows, Messages, Classes, Controls {D5}, DBCtrls,
  {$IFDEF USEJVCL}
  JvDBFindEdit, JVCLVer;
  {$ENDIF USEJVCL}

type
  TJvDotNetDBEdit = class(TDBEdit)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetDBListBox = class(TDBListBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetDBLookupListBox = class(TDBLookupListBox)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetDBMemo = class(TDBMemo)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  TJvDotNetDBRichEdit = class(TDBRichEdit)
  private
    {$IFDEF USEJVCL}
    FAboutJVCL: TJVCLAboutInfo;
    {$ENDIF USEJVCL}
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$IFDEF USEJVCL}
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  {$ENDIF USEJVCL}
  end;

  {$IFDEF USEJVCL}
  TJvDotNetDBFindEdit = class(TJvDBFindEdit)
  private
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  {$ENDIF USEJVCL}

implementation

uses
  JvDotNetUtils;

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

{$IFDEF USEJVCL}

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

{$ENDIF USEJVCL}
{$ENDIF DelphiPersonalEdition}

end.
