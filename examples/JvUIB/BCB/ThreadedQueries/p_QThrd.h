//---------------------------------------------------------------------------

#ifndef p_QThrdH
#define p_QThrdH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include "JvComponent.hpp"
#include "JvUIB.hpp"
//---------------------------------------------------------------------------
class TMyThread : public TThread
{            
private:
protected:
        void __fastcall Execute();
public:
        __fastcall TMyThread(bool CreateSuspended);
        virtual __fastcall ~TMyThread(void);
};
//---------------------------------------------------------------------------
#endif
