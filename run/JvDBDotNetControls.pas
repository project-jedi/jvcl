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

Last Modified: 2004-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDBDotNetControls;

{$IFDEF DelphiPersonalEdition}

interface

implementation

{$ELSE}

interface

uses
  Forms, Classes, Windows, Messages, Graphics, Controls, StdCtrls,
  ComCtrls, Mask, DBCtrls,
  {$IFDEF USEJVCL}
  CheckLst,
  JVCLVer;
  {$ELSE}
  CheckLst;
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

implementation

uses
  JvDotNetUtils;

//=== TJvDotNetDBEdit ========================================================

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBEdit.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBEdit.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetDBEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBEdit.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetDBEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== TJvDotNetDBListBox =====================================================

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBListBox.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetDBListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBListBox.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetDBListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBListBox.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetDBListBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== TJvDotNetDBLookupListBox ===============================================

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBLookupListBox.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetDBLookupListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBLookupListBox.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetDBLookupListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBLookupListBox.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetDBLookupListBox.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== TJvDotNetDBMemo ========================================================

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBMemo.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetDBMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBMemo.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetDBMemo.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBMemo.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetDBMemo.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

//=== TJvDotNetDBRichEdit ====================================================

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBRichEdit.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetDBRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBRichEdit.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetDBRichEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetDBRichEdit.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Msg: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetDBRichEdit.InternalWindowProc(var Msg: TMessage);
begin
  FOldWindowProc(Msg);
  DotNetMessageHandler(Msg, Self, Color, FHighlighted);
end;

{$ENDIF DelphiPersonalEdition}

end.
