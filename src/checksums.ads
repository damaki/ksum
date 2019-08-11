with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Configurations;        use Configurations;
with File_CSHAKE;
with File_Hashing;
with File_K12;
with File_KMAC;
with File_ParallelHash;
with File_XOF;
with Keccak.Types;          use Keccak.Types;
with Diagnostics;           use Diagnostics;

with CSHAKE;
with KangarooTwelve;
with MarsupilamiFourteen;
with KMAC;
with Parallel_Hash;
with RawSHAKE;
with SHA3;
with SHAKE;

package Checksums
is

   package K12_File_Hashing is new File_K12 (KangarooTwelve.K12);
   package M14_File_Hashing is new File_K12 (MarsupilamiFourteen.M14);

   package ParallelHash128_File_Hashing is new File_ParallelHash (Parallel_Hash.ParallelHash128);
   package ParallelHash256_File_Hashing is new File_ParallelHash (Parallel_Hash.ParallelHash256);

   package KMAC128_File_Hashing is new File_KMAC (KMAC.KMAC128);
   package KMAC256_File_Hashing is new File_KMAC (KMAC.KMAC256);

   package SHA3_224_File_Hashing is new File_Hashing (SHA3.SHA3_224);
   package SHA3_256_File_Hashing is new File_Hashing (SHA3.SHA3_256);
   package SHA3_384_File_Hashing is new File_Hashing (SHA3.SHA3_384);
   package SHA3_512_File_Hashing is new File_Hashing (SHA3.SHA3_512);

   package Keccak_224_File_Hashing is new File_Hashing (SHA3.Keccak_224);
   package Keccak_256_File_Hashing is new File_Hashing (SHA3.Keccak_256);
   package Keccak_384_File_Hashing is new File_Hashing (SHA3.Keccak_384);
   package Keccak_512_File_Hashing is new File_Hashing (SHA3.Keccak_512);

   package RawSHAKE128_File_Hashing is new File_XOF (RawSHAKE.RawSHAKE128);
   package RawSHAKE256_File_Hashing is new File_XOF (RawSHAKE.RawSHAKE256);

   package SHAKE128_File_Hashing is new File_XOF (SHAKE.SHAKE128);
   package SHAKE256_File_Hashing is new File_XOF (SHAKE.SHAKE256);

   package CSHAKE128_File_Hashing is new File_CSHAKE
     (CSHAKE.CSHAKE128,
      SHAKE128_File_Hashing);

   package CSHAKE256_File_Hashing is new File_CSHAKE
     (CSHAKE.CSHAKE256,
      SHAKE256_File_Hashing);

   type Hash_File_Procedure_Access is access procedure
     (File   : in     Ada.Text_IO.File_Type;
      Buffer : in out Keccak.Types.Byte_Array);

   type Check_File_Procedure_Access is access procedure
     (File          : in     Ada.Text_IO.File_Type;
      Buffer        : in out Keccak.Types.Byte_Array;
      Expected_Hash : in     Keccak.Types.Byte_Array;
      Result        :    out Diagnostic);

   Hash_File_Procs : constant array (Algorithm_Names) of Hash_File_Procedure_Access :=
     (Configurations.CSHAKE128           => CSHAKE128_File_Hashing.Hash_File'Access,
      Configurations.CSHAKE256           => CSHAKE256_File_Hashing.Hash_File'Access,
      Configurations.KangarooTwelve      => K12_File_Hashing.Hash_File'Access,
      Configurations.MarsupilamiFourteen => M14_File_Hashing.Hash_File'Access,
      Configurations.Keccak_224          => Keccak_224_File_Hashing.Hash_File'Access,
      Configurations.Keccak_256          => Keccak_256_File_Hashing.Hash_File'Access,
      Configurations.Keccak_384          => Keccak_384_File_Hashing.Hash_File'Access,
      Configurations.Keccak_512          => Keccak_512_File_Hashing.Hash_File'Access,
      Configurations.KMAC128             => KMAC128_File_Hashing.Hash_File'Access,
      Configurations.KMAC256             => KMAC256_File_Hashing.Hash_File'Access,
      Configurations.ParallelHash128     => ParallelHash128_File_Hashing.Hash_File'Access,
      Configurations.ParallelHash256     => ParallelHash256_File_Hashing.Hash_File'Access,
      Configurations.RawSHAKE128         => RawSHAKE128_File_Hashing.Hash_File'Access,
      Configurations.RawSHAKE256         => RawSHAKE256_File_Hashing.Hash_File'Access,
      Configurations.SHA3_224            => SHA3_224_File_Hashing.Hash_File'Access,
      Configurations.SHA3_256            => SHA3_256_File_Hashing.Hash_File'Access,
      Configurations.SHA3_384            => SHA3_384_File_Hashing.Hash_File'Access,
      Configurations.SHA3_512            => SHA3_512_File_Hashing.Hash_File'Access,
      Configurations.SHAKE128            => SHAKE128_File_Hashing.Hash_File'Access,
      Configurations.SHAKE256            => SHAKE256_File_Hashing.Hash_File'Access);

   Check_File_Procs : constant array (Algorithm_Names) of Check_File_Procedure_Access :=
     (Configurations.CSHAKE128           => CSHAKE128_File_Hashing.Check_File'Access,
      Configurations.CSHAKE256           => CSHAKE256_File_Hashing.Check_File'Access,
      Configurations.KangarooTwelve      => K12_File_Hashing.Check_File'Access,
      Configurations.MarsupilamiFourteen => M14_File_Hashing.Check_File'Access,
      Configurations.Keccak_224          => Keccak_224_File_Hashing.Check_File'Access,
      Configurations.Keccak_256          => Keccak_256_File_Hashing.Check_File'Access,
      Configurations.Keccak_384          => Keccak_384_File_Hashing.Check_File'Access,
      Configurations.Keccak_512          => Keccak_512_File_Hashing.Check_File'Access,
      Configurations.KMAC128             => KMAC128_File_Hashing.Check_File'Access,
      Configurations.KMAC256             => KMAC256_File_Hashing.Check_File'Access,
      Configurations.ParallelHash128     => ParallelHash128_File_Hashing.Check_File'Access,
      Configurations.ParallelHash256     => ParallelHash256_File_Hashing.Check_File'Access,
      Configurations.RawSHAKE128         => RawSHAKE128_File_Hashing.Check_File'Access,
      Configurations.RawSHAKE256         => RawSHAKE256_File_Hashing.Check_File'Access,
      Configurations.SHA3_224            => SHA3_224_File_Hashing.Check_File'Access,
      Configurations.SHA3_256            => SHA3_256_File_Hashing.Check_File'Access,
      Configurations.SHA3_384            => SHA3_384_File_Hashing.Check_File'Access,
      Configurations.SHA3_512            => SHA3_512_File_Hashing.Check_File'Access,
      Configurations.SHAKE128            => SHAKE128_File_Hashing.Check_File'Access,
      Configurations.SHAKE256            => SHAKE256_File_Hashing.Check_File'Access);

   procedure Print_Checksums;

   procedure Check_Checksums;

end Checksums;
