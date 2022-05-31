declare global {
  const optimizer: {
    simpleOptimizedFromString(code: string): string;
    simpleFromString(code: string): string;
    simpleAvailFromString(code: string): string;
    simpleLiveFromString(code: string): string;
    basicBlockFromString(code: string): string;
    ssaFromString(code: string): string;
  };
}

export {};
