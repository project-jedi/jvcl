{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDotNetCtrls.PAS, released on 2004-01-01.

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

unit JvDotNetControls;

interface

uses
  Forms, Classes, Windows, Messages, Graphics, Controls, StdCtrls,
  ComCtrls, Mask, CheckLst;

type
{ TJvDotNetCheckListBox }

  TJvDotNetCheckListBox = class(TCheckListBox)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetEdit }

  TJvDotNetEdit = class(TEdit)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


{ TJvDotNetHotKey }

  TJvDotNetHotKey = class(THotKey)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetListBox }

  TJvDotNetListBox = class(TListBox)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


{ TJvDotNetListView }

  TJvDotNetListView = class(TListView)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetMaskEdit }

  TJvDotNetMaskEdit = class(TMaskEdit)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetMemo }

  TJvDotNetMemo = class(TMemo)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetRichEdit }

  TJvDotNetRichEdit = class(TRichEdit)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


{ TJvDotNetScrollBox }

  TJvDotNetScrollBox = class(TScrollBox)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetTreeView }

  TJvDotNetTreeView = class(TTreeView)
  private
    { Private declarations }
    FHighlighted: Boolean;
    FOldWindowProc: TWndMethod;
    procedure InternalWindowProc(var Message: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TJvDotNetCustomControl }

(* TJvDotNetCustomControl = class(TWinControl)
  published
    { Published declarations }
    property Color;
  end;
*)
implementation
uses
  JvDotNetUtils;



{ TJvDotNetCheckListBox }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetCheckListBox.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetCheckListBox.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetCheckListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetCheckListBox.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetCheckListBox.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;

{ TJvDotNetEdit }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetEdit.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetEdit.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetEdit.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetEdit.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;


{ TJvDotNetHotKey }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetHotKey.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetHotKey.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetHotKey.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetHotKey.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetHotKey.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;

{ TJvDotNetListBox }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetListBox.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetListBox.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetListBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetListBox.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetListBox.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;


{ TJvDotNetListView }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetListView.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetListView.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetListView.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetListView.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetListView.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;

{ TJvDotNetMaskEdit }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetMaskEdit.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetMaskEdit.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetMaskEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetMaskEdit.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetMaskEdit.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;

{ TJvDotNetMemo }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetMemo.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetMemo.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetMemo.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetMemo.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetMemo.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;


{ TJvDotNetRichEdit }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetRichEdit.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetRichEdit.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetRichEdit.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetRichEdit.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetRichEdit.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;


{ TJvDotNetScrollBox }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetScrollBox.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetScrollBox.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetScrollBox.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetScrollBox.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetScrollBox.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;

{ TJvDotNetTreeView }

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetTreeView.Create
  Author:    mh
  Date:      25-Jun-2002
  Arguments: AOwner: TComponent
  Result:    None
-----------------------------------------------------------------------------}

constructor TJvDotNetTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldWindowProc := WindowProc;
  WindowProc := InternalWindowProc;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetTreeView.Destroy
  Author:    mh
  Date:      25-Jun-2002
  Arguments: None
  Result:    None
-----------------------------------------------------------------------------}

destructor TJvDotNetTreeView.Destroy;
begin
  WindowProc := FOldWindowProc;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------
  Procedure: TJvDotNetTreeView.InternalWindowProc
  Author:    mh
  Date:      25-Jun-2002
  Arguments: var Message: TMessage
  Result:    None
-----------------------------------------------------------------------------}

procedure TJvDotNetTreeView.InternalWindowProc(var Message: TMessage);
begin
  FOldWindowProc(Message);
  DotNetMessageHandler(Message, Self, Color, FHighlighted);
end;

end.
