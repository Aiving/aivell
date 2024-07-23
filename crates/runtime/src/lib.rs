//! Contains the runtime and core library for the Koto language

#![warn(missing_docs)]

mod display_context;
mod error;
mod io;
mod types;
mod vm;

pub mod core_lib;
pub mod prelude;
mod send_sync;

pub use crate::{
    display_context::DisplayContext,
    error::{type_error, type_error_with_slice, Error, ErrorFrame, ErrorKind, Result},
    io::{BufferedFile, DefaultStderr, DefaultStdin, DefaultStdout, KotoFile, KotoRead, KotoWrite},
    send_sync::{KotoSend, KotoSync},
    types::{
        AsyncKotoFunction, BinaryOp, CallContext, IsIterable, KAsyncNativeFunction,
        KCaptureFunction, KFunction, KIterator, KIteratorOutput, KList, KMap, KNativeFunction,
        KNumber, KObject, KRange, KString, KTuple, KValue, KotoCopy, KotoEntries, KotoFunction,
        KotoHasher, KotoIterator, KotoObject, KotoType, MetaKey, MetaMap, MethodContext, UnaryOp,
        ValueKey, ValueMap, ValueVec,
    },
    vm::{CallArgs, KotoVm, KotoVmSettings, ModuleImportedCallback, ReturnOrYield},
};
pub use deriver;
pub use memory::{make_ptr, make_ptr_mut, Borrow, BorrowMut, KCell, Ptr, PtrMut};

#[cfg(test)]
mod tests {
    use crate::Result;
    use bytecode::{Chunk, CompilerSettings, Loader};
    use memory::{make_ptr, make_ptr_mut, Borrow, KCell, Ptr, PtrMut};

    use crate::{KString, KotoFile, KotoRead, KotoVm, KotoVmSettings, KotoWrite};

    /// Captures output from Koto in a String
    ///
    /// [KotoWrite] is implemented for OutputCapture, allowing it to be used as stdout and stderr
    /// for the Koto runtime.
    #[derive(Clone, Debug)]
    pub struct OutputCapture {
        output: PtrMut<String>,
    }

    impl Default for OutputCapture {
        fn default() -> Self {
            Self {
                output: make_ptr_mut!(String::default()),
            }
        }
    }

    impl OutputCapture {
        /// Returns a [KotoVm] with `stdout` and `stderr` captured by an instance of [OutputCapture]
        pub fn make_vm_with_output_capture() -> (KotoVm, Self) {
            let output_capture = Self::default();

            let vm = KotoVm::with_settings(KotoVmSettings {
                stdout: make_ptr!(output_capture.clone()),
                stderr: make_ptr!(output_capture.clone()),
                ..Default::default()
            });

            (vm, output_capture)
        }

        /// Clears the captured output
        pub fn clear(&mut self) {
            self.output.borrow_mut().clear();
        }

        /// Returns the currently captured output
        pub fn captured_output(&self) -> Borrow<String> {
            self.output.borrow()
        }
    }

    impl KotoFile for OutputCapture {
        fn id(&self) -> KString {
            "_output_capture_".into()
        }
    }

    impl KotoRead for OutputCapture {}
    impl KotoWrite for OutputCapture {
        fn write(&self, bytes: &[u8]) -> Result<()> {
            let bytes_str = match std::str::from_utf8(bytes) {
                Ok(s) => s,
                Err(e) => return Err(e.to_string().into()),
            };
            self.output.borrow_mut().push_str(bytes_str);
            Ok(())
        }

        fn write_line(&self, output: &str) -> Result<()> {
            let mut unlocked = self.output.borrow_mut();
            unlocked.push_str(output);
            unlocked.push('\n');
            Ok(())
        }

        fn flush(&self) -> Result<()> {
            Ok(())
        }
    }

    #[test]
    fn testa() {
        let script = "var myvar = 123;\nprint(myvar);print(\"ббляяя реально ворк?\")";

        let (mut vm, output) = OutputCapture::make_vm_with_output_capture();

        let print_chunk = |script: &str, chunk: Ptr<Chunk>| {
            println!("{script}\n");
            let script_lines = script.lines().collect::<Vec<_>>();

            println!("Constants\n---------\n{}\n", chunk.constants);
            println!(
                "Instructions\n------------\n{}",
                Chunk::instructions_as_string(chunk, &script_lines)
            );
        };

        let mut loader = Loader::default();

        let chunk = match loader.compile_script(script, None, CompilerSettings::default()) {
            Ok(chunk) => chunk,
            Err(error) => {
                print_chunk(script, vm.chunk());
                panic!("Error while compiling script: {error}");
            }
        };

        match vm.run(chunk) {
            Ok(_) => {
                println!("{}", output.captured_output().as_str());
            }
            Err(e) => {
                print_chunk(script, vm.chunk());
                panic!("Error while running script: {e}");
            }
        }
    }
}
