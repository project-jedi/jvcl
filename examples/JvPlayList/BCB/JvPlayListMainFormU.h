//---------------------------------------------------------------------------

#ifndef JvPlayListMainFormUH
#define JvPlayListMainFormUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "JvExStdCtrls.hpp"
#include "JvListBox.hpp"
#include "JvPlaylist.hpp"
#include <ActnList.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TJvPlayListMainForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TJvPlaylist *JvPlayList1;
        TOpenDialog *OpenDialog1;
        TMainMenu *MainMenu1;
        TMenuItem *File1;
        TMenuItem *Open1;
        TMenuItem *Delete1;
        TMenuItem *N1;
        TMenuItem *Exit1;
        TMenuItem *Options1;
        TMenuItem *ShowNumbers1;
        TMenuItem *ShowExtensions1;
        TMenuItem *ShowDrives1;
        TMenuItem *Operations1;
        TMenuItem *DeleteDeadFiles1;
        TMenuItem *Delete2;
        TMenuItem *SortByPath1;
        TMenuItem *SortByPathInverted1;
        TMenuItem *SortBySongName1;
        TMenuItem *SortBySongNameInverted1;
        TMenuItem *N2;
        TMenuItem *RandomOrder1;
        TMenuItem *ReverseOrder1;
        TMenuItem *Selection1;
        TMenuItem *SelectAll1;
        TMenuItem *UnselectAll1;
        TMenuItem *InverseSelection1;
        TMenuItem *N3;
        TMenuItem *MoveSelectedUp1;
        TMenuItem *MoveSelectedDown1;
        TActionList *ActionList1;
        TAction *Open;
        TAction *Delete;
        TAction *Exit;
        TAction *DeleteDead;
        TAction *SortSong;
        TAction *SortPah;
        TAction *SortPathI;
        TAction *SortSongNameInverted;
        TAction *RandomOrder;
        TAction *Reverse;
        TAction *SelectAll;
        TAction *UnselectAll;
        TAction *InvSelect;
        TAction *MoveUp;
        TAction *MoveDown;
        void __fastcall OpenExecute(TObject *Sender);
        void __fastcall DeleteExecute(TObject *Sender);
        void __fastcall ExitExecute(TObject *Sender);
        void __fastcall DeleteDeadExecute(TObject *Sender);
        void __fastcall SortSongExecute(TObject *Sender);
        void __fastcall ShowDrives1Click(TObject *Sender);
        void __fastcall SortPahExecute(TObject *Sender);
        void __fastcall ShowNumbers1Click(TObject *Sender);
        void __fastcall ShowExtensions1Click(TObject *Sender);
        void __fastcall SortSongNameInvertedExecute(TObject *Sender);
        void __fastcall RandomOrderExecute(TObject *Sender);
        void __fastcall ReverseExecute(TObject *Sender);
        void __fastcall SelectAllExecute(TObject *Sender);
        void __fastcall UnselectAllExecute(TObject *Sender);
        void __fastcall InvSelectExecute(TObject *Sender);
        void __fastcall MoveUpExecute(TObject *Sender);
        void __fastcall MoveDownExecute(TObject *Sender);
        void __fastcall JvPlayList1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TJvPlayListMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TJvPlayListMainForm *JvPlayListMainForm;
//---------------------------------------------------------------------------
#endif
