unit cpuid;

interface

Function SupporteSEE:Boolean;Register;
Function SupporteSEE2:Boolean;Register;

implementation


Function LireFlagsProcesseur:Cardinal;Register;
// R�sultat dans EAX
Asm
  PUSH  EBX
  PUSH  EDI

  // Test de la possiblit� d'utiliser CPUID
  // Cette m�thode est directement tir�e de la doc Intel
  PUSHFD               // Obtentions des Etats actuels
  POP   EAX
  MOV   ECX,EAX        // Sauvegarde...
  XOR   EAX, 200000h   // Changement du bit ID
  PUSH  EAX            // Sauvegarde des Etats dans le proc
  POPFD                //
  PUSHFD               // Obtentions des Etats actuels
  POP   EAX            //
  XOR   EAX,ECX        // Test si le bit ID a �t� conserv�
  JE    @@ERREUR       // Fin si le processeur ne supporte pas CPUID

  // La "fonction" 0 demande le num�ro maximum de fonction
  // utilisable.
  MOV   EAX,0          // Fonctin 0 de CPUID
  CPUID
  CMP   EAX,1          // On veut la fonction 1 seulement
  JB    @@ERREUR       // Non support�e

  // Lecture des flags
  MOV   EAX,1          // Fonctin 1 de CPUID
  CPUID
  MOV   EAX,EDX        // Flags contenus dans EDX
  JMP   @@Fin

@@ERREUR:
  XOR   EAX,EAX

@@Fin:
  POP   EDI
  POP   EBX
End;



Function LireFlagsEtendusProcesseur: Cardinal; Register;
// R�sultat dans EAX
Asm
  PUSH  EBX
  PUSH  EDI

  // Test de la possiblit� d'utiliser CPUID
  // Cette m�thode est directement tir�e de la doc Intel
  PUSHFD               // Obtentions des Etats actuels
  POP   EAX
  MOV   ECX,EAX        // Sauvegarde...
  XOR   EAX, 200000h   // Changement du bit ID
  PUSH  EAX            // Sauvegarde des Etats dans le proc
  POPFD                //
  PUSHFD               // Obtentions des Etats actuels
  POP   EAX            //
  XOR   EAX,ECX        // Test si le bit ID a �t� conserv�
  JE    @@ERREUR       // Fin si le processeur ne supporte pas CPUID

  // La "fonction" 0 demande le num�ro maximum de fonction
  // utilisable.
  MOV   EAX,0          // Fonctin 0 de CPUID
  CPUID
  CMP   EAX,$80000001  // On veut la fonction �tendue $80000001
  JB    @@ERREUR       // Non support�e

  // Lecture des flags
  MOV   EAX,$80000001  // Fonctin 1 de CPUID
  CPUID
  MOV   EAX,EDX        // Flags contenus dans EDX
  JMP   @@Fin

@@ERREUR:
  XOR   EAX,EAX

@@Fin:
  POP   EDI
  POP   EBX
End;

Function SupporteSEE: Boolean; Register;
Asm
  CALL  LireFlagsProcesseur // Lecture des flags
  SHR   EAX,25              // On ne garde que le bit 25
  AND   EAX,1               // dans le bit 0 de EAX
End;

Function SupporteSEE2: Boolean; Register;
Asm
  CALL  LireFlagsProcesseur // Lecture des flags
  SHR   EAX,26              // On ne garde que le bit 26
  AND   EAX,1               // dans le bit 0 de EAX
End;
end.