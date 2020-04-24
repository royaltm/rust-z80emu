//! # [Cpu] trait is defined here.
mod debug;
mod flags;
mod parse;
mod registers;

use crate::host::*;
pub use debug::*;
pub use flags::*;
pub use parse::*;
pub use registers::*;

/// The Cpu trait provides means to execute and debug machine code or change the state of `self` at User's will.
pub trait Cpu: Clone + Default + PartialEq + Eq {
    /// Instantly resets the Cpu to its initial state.
    fn reset(&mut self);
    /// Returns the current value of the program counter.
    fn get_pc(&self) -> u16;
    /// Sets the current value of the program counter.
    fn set_pc(&mut self, pc: u16);
    /// Returns the current value of the stack pointer.
    fn get_sp(&self) -> u16;
    /// Sets the current value of the stack pointer.
    fn set_sp(&mut self, sp: u16);
    /// Returns the Accumulator value as an unsigned 8-bit integer.
    fn get_acc(&self) -> u8;
    /// Sets the Accumulator value from an unsigned 8-bit integer.
    fn set_acc(&mut self, val: u8);
    /// Returns the current state of the Flags register.
    fn get_flags(&self) -> CpuFlags;
    /// Sets the current state of the Flags register.
    fn set_flags(&mut self, flags: CpuFlags);
    /// Increases the memory refresh counter.
    fn inc_r(&mut self);
    /// Adds the arbitrary value to the memory refresh counter.
    /// This can be used to emulate the Cpu in the HALT state without executing the busy loop.
    fn add_r(&mut self, delta: i32);
    /// Returns the current value of the memory refresh register `R`.
    fn get_r(&self) -> u8;
    /// Sets the memory refresh register `R` value.
    fn set_r(&mut self, r: u8);
    /// Returns the current value of the interrupt page `I` register.
    fn get_i(&self) -> u8;
    /// Sets the current value of the interrupt page `I` register.
    fn set_i(&mut self, i: u8);
    /// Returns the current memory refresh address.
    fn get_ir(&self) -> u16;
    /// Returns values of interrupt flip-flops `(iff1, iff2)`.
    fn get_iffs(&self) -> (bool, bool);
    /// Sets the values of interrupt flip-flops.
    fn set_iffs(&mut self, iff1: bool, iff2: bool);
    /// Forces [Cpu] to enter the HALT state. This doesn't involve [Clock] and happens instantly.
    /// This also can be done by executing `HALT` instruction with [Cpu::execute_instruction].
    fn halt(&mut self);
    /// Returns `true` if the [Cpu] is in the HALT state.
    fn is_halt(&self) -> bool;
    /// Returns the current interrupt mode.
    fn get_im(&self) -> InterruptMode;
    /// Sets the interrupt mode.
    fn set_im(&mut self, im: InterruptMode);
    /// Swaps the `AF` register with its alternative counterpart `AF'`.
    fn ex_af_af(&mut self);
    /// Swaps the `BC`, `DE` and `HL` registers with their alternative counterparts `BC'`, `DE'` and `HL'`.
    fn exx(&mut self);
    /// Returns the content of the selected 8-bit register.
    ///
    /// The `reg` argument specifies the register. If the `prefix` argument is 
    /// one of [Prefix::Xdd] or [Prefix::Yfd] and the `reg` is [Reg8::H] or [Reg8::L]
    /// the content of the `IXh`, `IXl` or `IYh`, `IYl` will be returned instead.
    fn get_reg(&self, reg: Reg8, prefix: Option<Prefix>) -> u8;
    /// Sets the content of the selected 8-bit register.
    ///
    /// The `reg` argument specifies the register. If the `prefix` argument is 
    /// one of [Prefix::Xdd] or [Prefix::Yfd] and the `reg` is [Reg8::H] or [Reg8::L]
    /// the content of the `IXh`, `IXl` or `IYh`, `IYl` will be set instead.
    fn set_reg(&mut self, dst: Reg8, prefix: Option<Prefix>, val: u8);
    /// Returns the content of the selected pair of registers as a tuple of 8-bit unsigned integers.
    ///
    /// E.g. for [StkReg16::BC] the content of `(B, C)` will be returned.
    fn get_reg2(&self, src: StkReg16) -> (u8, u8);
    /// Returns the content of the selected pair of alternative registers as a tuple of 8-bit unsigned integers.
    ///
    /// E.g. for [StkReg16::AF] the content of `(A', F')` will be returned.
    fn get_alt_reg2(&self, src: StkReg16) -> (u8, u8);
    /// Returns the content of the selected pair of registers as an unsigned 16-bit integer.
    fn get_reg16(&self, src: StkReg16) -> u16;
    /// Returns the content of the selected pair of alternative registers as an unsigned 16-bit integer.
    fn get_alt_reg16(&self, src: StkReg16) -> u16;
    /// Sets the content of the selected pair of registers.
    fn set_reg16(&mut self, src: StkReg16, val: u16);
    /// Returns the content of one of the index registers as a tuple of 8-bit unsigned integers.
    ///
    /// Depending on `prefix` this will be:
    /// * [Prefix::Xdd] - `(IXh, IXl)`
    /// * [Prefix::Yfd] - `(IYh, IYl)`
    fn get_index2(&self, prefix: Prefix) -> (u8, u8);
    /// Returns the content of one of the index registers as a 16-bit unsigned integer.
    ///
    /// Depending on `prefix` this will be:
    /// * [Prefix::Xdd] - `IX`
    /// * [Prefix::Yfd] - `IY`
    fn get_index16(&self, prefix: Prefix) -> u16;
    /// Sets the content of one of the index registers.
    ///
    /// Depending on `prefix` this will be:
    /// * [Prefix::Xdd] - `IX`
    /// * [Prefix::Yfd] - `IY`
    fn set_index16(&mut self, prefix: Prefix, val: u16);
    /// Returns true if the Cpu will accept the interrupt request before executing the next opcode.
    fn is_irq_allowed(&self) -> bool;
    /// Returns true if the Cpu will accept the non-maskable interrupt before executing the next opcode.
    fn is_nmi_allowed(&self) -> bool;
    /// Restores the content of the `interrupt flip-flop 1` from the content of the `interrupt flip-flop 2`.
    /// This is what `RETN` instruction usually does.
    fn restore_iff1(&mut self);
    /// Disables the maskable interrupts by resetting both `interrupt flip-flops` to Off.
    ///
    /// This is what `DI` instruction usually does.
    fn disable_interrupts(&mut self);
    /// Enabes the maskable interrupts by setting both `interrupt flip-flops` to On.
    ///
    /// Prevents the interrupts to be allowed before the next command.
    /// This is what `EI` instruction usually does.
    fn enable_interrupts(&mut self);
    /// Returns `true` if the last command executed was `EI`.
    fn is_after_ei(&self) -> bool;
    /// Returns `true` if the last command executed was a `0xDD` or a `0xFD` prefix.
    ///
    /// See [Cpu::execute_instruction] for more information.
    fn is_after_prefix(&self) -> bool;
    /// Returns the prefix value after executing the last command.
    ///
    /// See [Cpu::execute_instruction] for more information.
    fn get_prefix(&self) -> Option<Prefix>;
    /// Requests a maskable interrupt.
    ///
    /// This is the alternative method to invoke the maskable interrupt. Usually while instructions
    /// are being executed the [Cpu] checks via [Io::is_irq] method if the interrrupt from any device is being
    /// requested.
    ///
    /// Returns `None` if the interrupt could not be accepted at this time. In this instance the method performs
    /// no operation.
    ///
    /// Returns `Some(Ok(()))` if an interrupt was accepted and no break was requested by the executed instruction.
    /// In this instance at least one instruction will be executed. Depending on the interrupt mode this would be:
    /// * [InterruptMode::Mode0] an instruction provided via [Io::irq_data].
    /// * [InterruptMode::Mode1] a `RST 38h` instruction.
    /// * [InterruptMode::Mode2] a hypothetical `PUSH pc + JP <vector address>` instruction.
    ///   A debugger will see the `JP` command in this instance.
    ///   TODO: perhaps this should be some special case mnemonic instead.
    ///
    /// The [Clock] is advanced by the `IRQ:6` cycle + optional wait states + cycles specific to the executed
    /// instruction (minus the `M1:4` cycle).
    ///
    /// `Some(Err(BreakCause))` indicates that an instruction requested a break. Currently this may be possible
    /// in the interrupt mode 0 when the `HALT`, `OUT` or `RETI` instruction was executed and the [Io::write_io]
    /// or [Io::reti] requested to break the execution.
    ///
    /// See [Cpu::execute_instruction] for the `debug` argument description.
    ///
    /// # Note
    ///
    /// If the interrupt is being accepted this method resets the `HALT` state before everything else.
    fn irq<M, T, F>(
        &mut self,
        control: &mut M,
        tsc: &mut T,
        debug: Option<F>
    ) -> Option<Result<M::WrIoBreak, M::RetiBreak>>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug);
    /// Attempts to trigger a non-maskable interrupt.
    ///
    /// Returns `false` if the interrupt could not be accepted at this time. The non-maskable interrupt
    /// is not being accepted in some situations, e.g. right after executing the `EI` instruction or after
    /// one of the `0xDD` and `0xFD` opcode prefixes. In this instance the method performs no operation.
    ///
    /// Returns `true` on success. In this instance no instruction will be executed but the program counter
    /// will be set to `0x0066` and the previous program counter will be pushed on the machine stack.
    /// The `interrupt flip-flop 1` is being set to `false`, while preserving the value of `iff 2` and the
    /// [Clock] advances according to the Z80 NMI cycles: `M1:4 + IR:1 + SP-1:3 + SP-2:3`.
    ///
    /// # Note
    ///
    /// If the interrupt is being accepted this method resets the `HALT` state before everything else.
    fn nmi<M, T>(&mut self, control: &mut M, tsc: &mut T) -> bool
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock;
    /// Executes a single instruction given as `code`. If the instruction is a first byte of the multi-byte
    /// instruction the rest of the instruction body will be fetched via calls to [Memory::read_opcode].
    ///
    /// The return value `Err(BreakCause)` indicates that an instruction requested a break.
    /// Currently this may be possible when the `HALT` instruction was executed or when the `OUT` family
    /// instruction was executed and the [Io::write_io] requested a break or the `RETI` instruction was
    /// executed and the [Io::reti] requested a break. See also [BreakCause].
    ///
    /// If `debug` argument is `Some(F)`, a closure `F` may be called with [CpuDebug] argument during the
    /// instruction execution. It won't be called if `code` is one of the `0xDD` or `0xFD` prefixes.
    ///
    /// There is no limit of how many of these prefixes can be present before an actual instruction, so
    /// the execution is finished each time one of them is being encountered to prevent the overfeeding the [Clock]
    /// with a huge amount of T-states and to allow for the better synchronization with the emulated side effects.
    /// The user can check with [Cpu::is_after_prefix] method if this is the case after calling one of the
    /// execution methods.
    ///
    /// # Note
    ///
    /// This method resets the `HALT` state and `after EI` state before the instruction is being executed.
    fn execute_instruction<M, T, F>(
        &mut self,
        control: &mut M,
        tsc: &mut T,
        debug: Option<F>,
        code: u8
    ) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug);
    /// Executes the next instruction present in the [Memory] at the program counter fetched via [Memory::read_opcode].
    ///
    /// If interrupts are allowed, before fetching the instruction, checks if the interrupt request is present
    /// via [Io::is_irq] and enters the interrupted state instead of fetching and executing the next instruction.
    ///
    /// If the Cpu is in the `HALT` state, increases the memory refresh register and advances the [Clock] only.
    /// If `debug` closure is given, it will not be called in this instance.
    ///
    /// See [Cpu::execute_instruction] and [Cpu::irq] for the returned value and `debug` argument descriptions.
    fn execute_next<M, T, F>(
        &mut self,
        control: &mut M,
        tsc: &mut T,
        debug: Option<F>
    ) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock,
          F: FnOnce(CpuDebug);
    /// Executes instructions until [Clock] reaches the given `limit` or when other conditions are met.
    ///
    /// Returns `Ok(())` only when `limit` has been reached and the last executed instruction didn't request a break.
    ///
    /// Returns `Err(BreakCause)` when:
    /// * A `HALT` instruction was encountered.
    /// * An instruction requested a break via [Io::write_io] or [Io::reti].
    ///
    /// See also [BreakCause].
    ///
    /// Before fetching each next instruction this method checks if the interrupt has been requested via [Io::is_irq]
    /// and executes the interrupt routines without breaking the execution.
    ///
    /// When called while in the `HALT` state, increases the memory refresh register and advances the [Clock]
    /// until the `limit` has been reached. If interrupts were enabled and an interrupt was requested via
    /// [Io::is_irq] the `HALT` state is being reset and the regular execution of commands will be resumed.
    fn execute_with_limit<M, T>(
        &mut self,
        control: &mut M,
        tsc: &mut T,
        limit: T::Limit
    ) -> Result<M::WrIoBreak, M::RetiBreak>
    where M: Memory<Timestamp=T::Timestamp> + Io<Timestamp=T::Timestamp>,
          T: Clock;
}
