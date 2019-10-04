using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace AndroidJNIConstructor
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class AndroidJNIConstructorAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "AndroidJNIConstructor";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
       // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(AnalyzerResources.Title),AnalyzerResources.ResourceManager,typeof(AnalyzerResources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(AnalyzerResources.MessageFormat), AnalyzerResources.ResourceManager,typeof(AnalyzerResources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(AnalyzerResources.Description), AnalyzerResources.ResourceManager, typeof(AnalyzerResources));
        private const string Category = "Class";

        private static DiagnosticDescriptor Rule = 
            new DiagnosticDescriptor(
                DiagnosticId,
                Title, 
                MessageFormat, 
                Category, 
                DiagnosticSeverity.Error, 
                isEnabledByDefault: true, 
                description: Description
            );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterCompilationStartAction(compilationContext =>
            {
                INamedTypeSymbol javaObjectType = compilationContext.Compilation.GetTypeByMetadataName("Java.Lang.Object");
                if (javaObjectType == null)
                {
                    return;
                }

                compilationContext.RegisterSemanticModelAction(AnalyzeSemanticModel);
            });
        }
        
        private void AnalyzeSemanticModel(SemanticModelAnalysisContext context)
        {
            if (context.SemanticModel.IsSpeculativeSemanticModel)
            {
                return;
            }

            var compilationUnit = (CSharpCompilation)context.SemanticModel.Compilation;
            INamedTypeSymbol javaObjectType = compilationUnit.GetTypeByMetadataName("Java.Lang.Object");
            INamedTypeSymbol intPtrType = compilationUnit.GetTypeByMetadataName("System.IntPtr");
            INamedTypeSymbol jniHandleOwnershipType = compilationUnit.GetTypeByMetadataName("Android.Runtime.JniHandleOwnership");

            List<SyntaxNode> classNodes = ListClass(context.SemanticModel.SyntaxTree.GetRoot()).ToList();

            foreach(var node in classNodes)
            {
                Console.WriteLine(node);

                ClassDeclarationSyntax classDeclarationSyntax = (ClassDeclarationSyntax)node;
                INamedTypeSymbol declaredSymbol = context.SemanticModel.GetDeclaredSymbol(classDeclarationSyntax);

                if(!InheritFrom(declaredSymbol, javaObjectType))
                {
                    continue;
                }

                if(!HasJNIConstructor(declaredSymbol.BaseType, intPtrType, jniHandleOwnershipType))
                {
                    continue;
                }

                if (HasJNIConstructor(classDeclarationSyntax, context, intPtrType, jniHandleOwnershipType))
                {
                    continue;
                }

                var tokens = classDeclarationSyntax.ChildTokens().ToList();
                var childNodex = classDeclarationSyntax.ChildNodes().ToList();

                var locations = declaredSymbol.Locations;

                foreach(var location in locations)
                {
                    context.ReportDiagnostic(Diagnostic.Create(Rule, location, declaredSymbol.Name));
                }
            }
        }

        private bool HasJNIConstructor(INamedTypeSymbol type, INamedTypeSymbol intPtrType, INamedTypeSymbol jniHandleOwnershipType)
        {
            return type
                .GetMembers()
                .Any(member =>
                    member.Kind == SymbolKind.Method && 
                    member is IMethodSymbol method && 
                    method.MethodKind == MethodKind.Constructor && 
                    method.Parameters.Length == 2 &&
                    IsType(method.Parameters[0], intPtrType) &&
                    IsType(method.Parameters[1], jniHandleOwnershipType)
                );
        }

        private bool InheritFrom(INamedTypeSymbol symbol, ITypeSymbol type)
        {
            var baseType = symbol.BaseType;
            while(baseType != null)
            {
                if (type.Equals(baseType))
                {
                    return true;
                }

                baseType = baseType.BaseType;
            }
            return false;
        }

        private bool HasJNIConstructor(ClassDeclarationSyntax classDeclaration, SemanticModelAnalysisContext context, INamedTypeSymbol intPtrType, INamedTypeSymbol jniHandleOwnershipType)
        {
            foreach(MemberDeclarationSyntax member in classDeclaration.Members)
            {
                if(member.IsKind(SyntaxKind.ConstructorDeclaration))
                {
                    var constructorDeclaration = member as ConstructorDeclarationSyntax;

                    if(constructorDeclaration.ParameterList.Parameters.Count == 2)
                    {
                        ParameterSyntax parameterIntPtr = constructorDeclaration.ParameterList.Parameters[0];
                        ParameterSyntax parameterJniHandleOwnership = constructorDeclaration.ParameterList.Parameters[1];

                        // Has correct parameters 
                        if(IsType(parameterIntPtr, intPtrType, context) && IsType(parameterJniHandleOwnership, jniHandleOwnershipType, context))
                        {
                            // Base is called correctly...
                            if(constructorDeclaration.Initializer is { } initializer && 
                                initializer.ThisOrBaseKeyword.IsKind(SyntaxKind.BaseKeyword) && 
                                initializer.ArgumentList is { } argumentList && 
                                argumentList.Arguments is { } arguments && 
                                arguments.Count == 2 && 
                                IsSameParameter(arguments[0], parameterIntPtr) &&
                                IsSameParameter(arguments[1], parameterJniHandleOwnership)
                                )
                            {
                                return true;
                            }

                        }
                    }
                }
            }

            return false;

            static bool IsSameParameter(ArgumentSyntax argument, ParameterSyntax parameter)
            {
                return argument.Expression is IdentifierNameSyntax argumentName &&
                    parameter.Identifier.ValueText == argumentName.Identifier.ValueText;
            }
        }
        
        private bool IsType(ParameterSyntax parameter, ITypeSymbol type, SemanticModelAnalysisContext context)
        {
            var parameterTypeInfo2 = context.SemanticModel.GetDeclaredSymbol(parameter);

            return type.Equals(parameterTypeInfo2.Type);
        }

        private bool IsType(IParameterSymbol parameter, ITypeSymbol type)
        {
            return type.Equals(parameter.Type);
        }

        private IEnumerable<SyntaxNode> ListClass(SyntaxNode node)
        {
            Queue<SyntaxNode> nodes = new Queue<SyntaxNode>();
            nodes.Enqueue(node);

            while(nodes.Count > 0)
            {
                var currentNode = nodes.Dequeue();
                if (currentNode.IsKind(SyntaxKind.ClassDeclaration))
                {
                    yield return currentNode;
                }

                foreach(var child in currentNode.ChildNodes())
                {
                    nodes.Enqueue(child);
                }
            }
        }
    }
}
